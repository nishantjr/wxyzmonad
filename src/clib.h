#pragma once

#ifndef WLR_USE_UNSTABLE
#define WLR_USE_UNSTABLE
#endif

#include <wlr/types/wlr_keyboard.h>

struct wlr_seat;
struct wxyz_toplevel;

struct wxyz_keyboard_key_event {
    struct wlr_keyboard_key_event event;
    xkb_keysym_t keysym;
    uint32_t modifiers;
    struct wlr_seat* seat;
};

struct wxyz_xdg_toplevel_map_event      { struct wxyz_toplevel* toplevel; };
struct wxyz_xdg_toplevel_unmap_event    { struct wxyz_toplevel* toplevel; };

enum wxyz_event_type {
    KEYBOARD_KEY = 1,
    XDG_TOPLEVEL_MAP,
    XDG_TOPLEVEL_UNMAP,
};
struct wxyz_event {
    enum wxyz_event_type type;
    union {
        struct wxyz_keyboard_key_event          keyboard_key;
        struct wxyz_xdg_toplevel_map_event      xdg_toplevel_map;
        struct wxyz_xdg_toplevel_unmap_event    xdg_toplevel_unmap;
    };
};

struct wxyz_event* wxyz_next_event();

int wxyz_init();
void wxyz_run();
void wxyz_shutdown();

void wxyz_terminate();
void focus_toplevel(struct wxyz_toplevel *toplevel);
void wxyz_next_toplevel();

