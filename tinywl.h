#pragma once

#ifndef WLR_USE_UNSTABLE
#define WLR_USE_UNSTABLE
#endif

#include <wlr/types/wlr_keyboard.h>

struct wlr_seat;

struct tinywl_keyboard_key_event {
    struct wlr_keyboard_key_event event;
    xkb_keysym_t keysym;
    uint32_t modifiers;
    struct wlr_seat* seat;
};

struct tinywl_keyboard_key_event* wxyz_next_event();

int wxyz_init();
void wxyz_run();
void wxyz_shutdown();

void wxyz_terminate();
void wxyz_next_toplevel();



