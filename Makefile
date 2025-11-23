PKG_CONFIG?=pkg-config
WAYLAND_PROTOCOLS!=$(PKG_CONFIG) --variable=pkgdatadir wayland-protocols
WAYLAND_SCANNER!=$(PKG_CONFIG) --variable=wayland_scanner wayland-scanner

PKGS="wlroots-0.19" wayland-server xkbcommon
CFLAGS_PKG_CONFIG!=$(PKG_CONFIG) --cflags $(PKGS)
CFLAGS+=$(CFLAGS_PKG_CONFIG)
LIBS!=$(PKG_CONFIG) --libs $(PKGS)

all: tinywl

# wayland-scanner is a tool which generates C headers and rigging for Wayland
# protocols, which are specified in XML. wlroots requires you to rig these up
# to your build system yourself and provide them in the include path.
xdg-shell-protocol.h:
	$(WAYLAND_SCANNER) server-header \
		$(WAYLAND_PROTOCOLS)/stable/xdg-shell/xdg-shell.xml $@

%.o: %.hs
	ghc -main-is Main.main -c -O $< -DWLR_USE_UNSTABLE  $(CFLAGS) -o $@

%.hs: %.hsc
	hsc2hs -o $@ $< $(CFLAGS)

tinywl.o: tinywl.c xdg-shell-protocol.h
	ghc -c $< -g -Werror $(CFLAGS) -I. -o $@

tinywl: tinywl.o Config.o Main.o Layout.o Main.o StackSet.o Tiling.o WXYZMonad.o
	ghc --make $^ $> -g -Werror $(CFLAGS) $(LDFLAGS) $(LIBS) -o $@ -package containers -package process

clean:
	rm -f tinywl tinywl.o xdg-shell-protocol.h

.PHONY: all clean

# --- Additional Dependencies -----------------------------------------------

Config.o : WXYZMonad.o
Main.hsc : WXYZMonad.o Config.o
Layout.o : StackSet.o

