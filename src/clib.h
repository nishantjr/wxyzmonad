#pragma once

#ifndef WLR_USE_UNSTABLE
#define WLR_USE_UNSTABLE
#endif

#include <wlr/types/wlr_keyboard.h>
#include "wlr-layer-shell-unstable-v1-protocol.h"

struct wlr_seat;
struct wxyz_output;
struct wxyz_toplevel;
struct wxyz_layer_surface;

struct wxyz_keyboard_key_event {
    struct wlr_keyboard_key_event event;
    xkb_keysym_t keysym;
    uint32_t modifiers;
    struct wlr_seat* seat;
};

struct wxyz_xdg_toplevel_map_event      { struct wxyz_toplevel* toplevel; };
struct wxyz_xdg_toplevel_unmap_event    { struct wxyz_toplevel* toplevel; };

struct wxyz_layer_surface_map_event     { struct wxyz_layer_surface* surface; };
struct wxyz_layer_surface_unmap_event   { struct wxyz_layer_surface* surface; };

struct wxyz_output_new_event            { struct wxyz_output* output;
                                          int32_t width;
                                          int32_t height;
                                        };
struct wxyz_output_destroy_event        { struct wxyz_output* output; };
typedef struct wxyz_output_new_event    wxyz_output_request_state_event; // alias

enum wxyz_event_type {
    KEYBOARD_KEY = 1,

    XDG_TOPLEVEL_MAP,
    XDG_TOPLEVEL_UNMAP,

    LAYER_SURFACE_MAP,
    LAYER_SURFACE_UNMAP,

    OUTPUT_NEW,
    OUTPUT_DESTROY,
    OUTPUT_REQUEST_STATE,
};
struct wxyz_event {
    struct wl_list link;
    enum wxyz_event_type type;
    union {
        struct wxyz_keyboard_key_event          keyboard_key;

        struct wxyz_xdg_toplevel_map_event      xdg_toplevel_map;
        struct wxyz_xdg_toplevel_unmap_event    xdg_toplevel_unmap;

        struct wxyz_layer_surface_map_event      layer_surface_map;
        struct wxyz_layer_surface_unmap_event    layer_surface_unmap;

        struct wxyz_output_new_event            output_new;
        struct wxyz_output_destroy_event        output_destroy;
               wxyz_output_request_state_event  output_request_state;
    };
};

struct wxyz_event* wxyz_next_event();

int wxyz_init();
void wxyz_run();
void wxyz_shutdown();

void wxyz_terminate();
void focus_toplevel(struct wxyz_toplevel *toplevel);

void wxyz_toplevel_set_position(struct wxyz_toplevel*, int x, int y);
void wxyz_toplevel_set_size(struct wxyz_toplevel*, int width, int height);

void wxyz_layer_surface_set_size(struct wxyz_layer_surface *surface, int width, int height);
void wxyz_layer_surface_set_position(struct wxyz_layer_surface *surface, int width, int height);
