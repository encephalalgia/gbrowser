#include "ppu.hpp"

#include <utility>
#include <algorithm>
#include "interrupts.hpp"
#include "renderers/renderer.hpp"

template<console model>
void ppu<model>::tick(const u16 t)
{
    if (not(lcdc & lcd_enable_bit)) [[unlikely]] {
        if (was_lcd_enabled) {
            was_lcd_enabled = true;
            ly = dots = mode = 0;
            std::ranges::fill(buffer, color[0]);
        }

        return;
    }

    if (not was_lcd_enabled) [[unlikely]] {
        was_lcd_enabled = true;
        mode = 2;
    }

    dots += t;

    switch (mode) {
        case 2: // OAM scan
            if constexpr (model == console::dmg) {
                if (obj_count != 10) {
                    oam_scan_row(t);
                }
            }

            if (dots >= mode_2_len) {
                mode = 3;
                if constexpr (model != console::dmg) {
                    oam_scan_all();
                }
                calculate_mode_3_penalty();
            }
            break;
        case 3: // Drawing pixels
            if (dots >= mode_2_len + base_mode_3_len + mode_3_penalty) {
                mode = 0;
                draw_pixels();
            }
            break;
        case 0: // HBlank
            if (dots >= dots_per_scanline) {
                dots -= dots_per_scanline;

                if (++ly == screen_height) {
                    mode = 1;
                    interrupts.req(vblank_interrupt_bit);
                    renderer.render(buffer);
                    y_condition = false;
                }
                else {
                    mode = 2;
                    obj_count = 0;
                    scx_latched = scx;
                    y_condition = y_condition or wy == ly;
                }
            }
            break;
        case 1: // VBlank
            if (dots >= dots_per_scanline) {
                dots -= dots_per_scanline;

                if (++ly == screen_height + 10) {
                    mode = 2;
                    obj_count = 0;
                    scx_latched = scx;
                    ly = 0;
                }
            }
            break;
        default: std::unreachable();
    }
}

template<console model>
void ppu<model>::set_dma(u8 val) {
}

template<console model>
u8 ppu<model>::read_vram(const u16 addr) const
{
    return is_vram_accessible() ? vram[addr] : 0xFF;
}

template<console model>
void ppu<model>::write_vram(const u16 addr, const u8 data)
{
    if (is_vram_accessible()) {
        vram[addr] = data;
    }
}

template<console model>
u8 ppu<model>::read_oam(const u8 addr)
{
    if (is_oam_accessible()) {
        return oam[addr];
    }

    if constexpr (model == console::dmg) {
        corrupt_oam_rd();
    }

    return 0xFF;
}

template<console model>
void ppu<model>::write_oam(const u8 addr, const u8 data)
{
    if (is_oam_accessible()) {
        oam[addr] = data;
    }
    else {
        if constexpr (model == console::dmg) {
            corrupt_oam_wr();
        }
    }
}

template<console model>
bool ppu<model>::is_vram_accessible() const
{
    return mode < 3;
}

template<console model>
bool ppu<model>::is_oam_accessible() const
{
    return mode < 2;
}

template<console model>
void ppu<model>::corrupt_oam_rd()
{
    if (mode != 2) {
        return;
    }

    const u8 row{static_cast<u8>(dots / 4)};
    if (row == 0) {
        return;
    }

    const u16 a{get_oam_word<1>(row)};
    const u16 b{get_oam_word<1>(row - 1)};
    const u16 c{get_oam_word<3>(row - 1)};

    corrupt_oam_row(row, b | (a & c));
}

template<console model>
void ppu<model>::corrupt_oam_wr()
{
    if (mode != 2) {
        return;
    }

    const u8 row{static_cast<u8>(dots / 4)};
    if (row == 0) {
        return;
    }

    const u16 a{get_oam_word<1>(row)};
    const u16 b{get_oam_word<1>(row - 1)};
    const u16 c{get_oam_word<3>(row - 1)};

    corrupt_oam_row(row, ((a ^ c) & (b ^ c)) ^ c);
}

template<console model>
void ppu<model>::corrupt_oam_rw()
{
    if (mode != 2) {
        return;
    }

    const u8 row{static_cast<u8>(dots / 4)};

    if (row < 4 or row + 1 == 20) {
        return;
    }

    const u16 a{get_oam_word<1>(row - 2)};
    const u16 b{get_oam_word<1>(row - 1)};
    const u16 c{get_oam_word<1>(row)};
    const u16 d{get_oam_word<3>(row - 1)};

    const u16 p{static_cast<u16>((b & (a | c | d)) | (a & c & d))};
    oam[(row - 1) * 8] = p & 0xFF;
    oam[(row - 1) * 8 + 1] = p >> 8;

    for (int i{0}; i != 8; ++i) {
        oam[row * 8 + i] = oam[(row - 2) * 8 + i] = oam[(row - 1) * 8 + i];
    }
}

template<console model>
void ppu<model>::oam_scan_row(const u8 t)
{
    const u8 max_y{static_cast<u8>(ly + 16)};
    const u8 obj_height{static_cast<u8>(lcdc & obj_size_bit ? tile_width * 2 : tile_width)};

    for (int begin{(dots - t) / 2}, end{dots / 2}; begin != end and obj_count != 10; ++begin) {
        const int obj_index{begin * 4};

        if (const u8 y{oam[obj_index]}; max_y - obj_height < y and y <= max_y) {
            for (int i{0}; i != 4; ++i) {
                obj_buffer[(obj_count * 4) + i] = oam[obj_index + i];
            }

            ++obj_count;
        }
    }
}

template<console model>
void ppu<model>::oam_scan_all()
{
    const u8 max_y{static_cast<u8>(ly + 16)};
    const u8 obj_size{static_cast<u8>(lcdc & obj_size_bit ? tile_width * 2 : tile_width)};

    for (int obj_index{0}; obj_index != 0xA0; obj_index += 4) {
        if (const u8 y{oam[obj_index]}; max_y - obj_size < y and y <= max_y) {
            for (int i{0}; i != 4; ++i) {
                obj_buffer[(obj_count * 4) + i] = oam[obj_index + i];
            }

            ++obj_count;
        }
    }
}

template<console model>
template<u8 word_index>
u16 ppu<model>::get_oam_word(const u8 row) const
{
    constexpr u8 byte_index{(word_index - 1) * 2};
    return (oam[row * 8 + byte_index + 1] << 8) | oam[row * 8 + byte_index];
}

template<console model>
void ppu<model>::corrupt_oam_row(const u8 row, const u16 p)
{
    oam[row * 8] = p & 0xFF;
    oam[row * 8 + 1] = p >> 8;

    for (int i{2}; i != 8; ++i) {
        oam[row * 8 + i] = oam[(row - 1) * 8 + i];
    }
}

template<console model>
void ppu<model>::calculate_mode_3_penalty()
{
    int obj_penalty{0};

    if (lcdc & obj_enable_bit) {
        u64 tile_seen{0};

        for (int i{0}; i != obj_count; ++i) {
            const u8 x{obj_buffer[(i * 4) + 1]};

            if (x == 0) {
                obj_penalty += 11;
                continue;
            }

            const size_t screen_x{x - 8U};
            const bool is_window_tile{is_window_visible() and screen_x + 7U >= wx};
            const size_t offset_x{is_window_tile ? 7U - wx : scx};

            const size_t tile{((screen_x + offset_x) >> 3U) & (tiles_per_row - 1U)};
            const size_t tile_index{tile + (is_window_tile << 5U)};
            const u64 tile_bit{1ULL << tile_index};

            if (not(tile_seen & tile_bit)) {
                tile_seen |= tile_bit;
                obj_penalty += std::max(static_cast<int>(5 - (screen_x + offset_x) % tile_width), 0);
            }

            obj_penalty += 6;
        }
    }

    mode_3_penalty = (scx & 7) + 6 * is_window_visible() + obj_penalty;
}

template<console model>
void ppu<model>::draw_pixels()
{
    const size_t offset{tile_map_base + ((lcdc & bg_tile_map_bit) ? 0x400U : 0U)};
    const size_t tile_base{lcdc & tile_data_bit ? 0U : 0x800U};
    const size_t tile_mask_bit{lcdc & tile_data_bit ? 0U : 0x80U};
    const size_t base_px{screen_width * ly};

    if (lcdc & bg_and_window_bit) {
        size_t px{0};

        const size_t base_tile = (scy + ly) % tile_map_width / tile_width * tiles_per_row + offset;
        const size_t tile_row{(scy + ly) % tile_width * 2U};

        u8 tile{
            vram[(vram[(base_tile + scx) % tile_map_width / tile_width] ^ tile_mask_bit) * bytes_per_tile +
                 tile_base +
                 tile_row]
        };

        for (int i{scx % 8}; i != 8; ++i, ++px) {
            for (int j{0}; j != 4; ++j) {
                buffer[base_px + px] = tile[i + 1] << 1 | tile[i];
            }
            buffer[base_px + px] =
        }

        for (int i{1}; i != 21 and px != screen_width; ++i) {
            tile = ((scx + i * 8) % 256) / 8;
        }

        for int i = 0 to max
            col = scx + i % 256;
    }
}

template<console model>
u8 ppu<model>::get_tile_index(const int base, const int index) const
{
    return vram[base + (scx + index * 8) % 256];
}

template<console model>
bool ppu<model>::is_window_visible()
{
    if constexpr (model == console::dmg) {
        return (lcdc & bg_and_window_bit) and (lcdc & window_enable_bit) and y_condition;
    }
    else {
        return (lcdc & window_enable_bit) and y_condition;
    }
}

template class ppu<console::dmg>;
template class ppu<console::cgb>;
