all: configure_mate_session configure_xmonad
	echo Hopefully xmonad+mate is configured. Try logging out and picking the xmonad+mate session when logging in.
.PHONY: all

install_xmonad:
	sudo apt-get install xmonad libghc-xmonad-contrib-dev
.PHONY: install_xmonad

configure_mate_session:
	cp xmonad.desktop /usr/share/applications/xmonad.desktop
	cp xmonad-mate.desktop /usr/share/xsessions/xmonad-mate.desktop
	gsettings set org.mate.session.required-components windowmanager "'xmonad'"
.PHONY: configure_mate_session

configure_xmonad: install_xmonad
	mkdir -p ~/.xmonad
	cp xmonad.hs ~/.xmonad/xmonad.hs
	xmonad --recompile
.PHONY: configure_xmonad

