#pragma once

#include "types.hpp"

class interrupts;

class renderer;

template<console model>
class ppu {
public:
  ppu(renderer& r, interrupts& i)
    : renderer{r}, interrupts{i}
  {}

  u8 dma{model == console::dmg ? 0xFF : 0x00};
  u8 lcdc{0x91};
  u8 stat{model == console::dmg ? 0x85 : 0x00};
  u8 scy{0x00}, scx{0x00};
  u8 ly{0x00}, lyc{0x00};
  u8 bgp{0xFC};
  u8 obp0{0x00}, obp1{0x00};
  u8 wx{0x00}, wy{0x00};

  void tick(u16 t);

  void set_dma(u8 val);

  [[nodiscard]] u8 read_vram(u16 addr) const;

  void write_vram(u16 addr, u8 data);

  [[nodiscard]] u8 read_oam(u8 addr);

  void write_oam(u8 addr, u8 data);

  [[nodiscard]] bool is_vram_accessible() const;

  [[nodiscard]] bool is_oam_accessible() const;

  void corrupt_oam_rd();

  void corrupt_oam_wr();

  void corrupt_oam_rw();

private:
  static constexpr u16 mode_2_len{80};
  static constexpr u16 base_mode_3_len{172};
  static constexpr u16 dots_per_scanline{456};

  static constexpr u8 bytes_per_tile{16};
  static constexpr u8 tiles_per_row{32};
  static constexpr u8 tile_width{8};
  static constexpr u16 tile_map_width{256};

  static constexpr u8 lcd_enable_bit{0x80};
  static constexpr u8 window_tile_map_bit{0x40};
  static constexpr u8 window_enable_bit{0x20};
  static constexpr u8 tile_data_bit{0x10};
  static constexpr u8 bg_tile_map_bit{0x08};
  static constexpr u8 obj_size_bit{0x04};
  static constexpr u8 obj_enable_bit{0x02};
  static constexpr u8 bg_and_window_bit{0x01};

  static constexpr u8 lyc_interrupt_bit{0x40};
  static constexpr u8 oam_interrupt_bit{0x20};
  static constexpr u8 vblank_interrupt_bit{0x10};
  static constexpr u8 hblank_interrupt_bit{0x08};
  static constexpr u8 lyc_equal_ly_bit{0x04};
  static constexpr u8 ppu_mode{0x03};

  static constexpr size_t tile_map_base{0x1800};

  u32 color[4]{
    0xFF'9A'9E'3F, // 0 -> white
    0xFF'49'6B'22, // 1 -> light gray
    0xFF'0E'45'0B, // 2 -> dark gray
    0xFF'1B'2A'09, // 3 -> black
  };

  renderer& renderer;
  interrupts& interrupts;

  u8 vram[0x2000]{};
  u8 oam[0xA0]{};
  u32 buffer[screen_width * screen_height]{};

  u16 dots{0};
  u16 mode_3_penalty{0};
  u8 mode{2};

  u8 obj_buffer[10 * 4]{};
  u8 obj_count{0};

  u8 scx_latched{0};

  bool was_lcd_enabled{false};
  bool y_condition{false};

  void oam_scan_row(u8 t);

  void oam_scan_all();

  template<u8 word_index>
  [[nodiscard]] u16 get_oam_word(u8 row) const;

  void corrupt_oam_row(u8 row, u16 p);

  void calculate_mode_3_penalty();

  void draw_pixels();

  [[nodiscard]] u8 get_tile_index(int base, int index) const;

  bool is_window_visible();
};
