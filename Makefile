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

Config.o: Config.hs
	ghc -c -O $< -o $@
tinywl.o: tinywl.c xdg-shell-protocol.h Config.o
	ghc -c $< -g -Werror $(CFLAGS) -I. -o $@
tinywl: tinywl.o Config.o
	ghc --make -no-hs-main $^ $> -g -Werror $(CFLAGS) $(LDFLAGS) $(LIBS) -o $@
clean:
	rm -f tinywl tinywl.o xdg-shell-protocol.h

.PHONY: all clean
