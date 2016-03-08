
if [ "$1" == "restore" ]
  then SET_TO=$TMP_GTK_THEME_USED
  else 
  SET_TO='Adwaita'
  TMP=`gsettings get org.gnome.desktop.interface gtk-theme`
  if [ "$TMP" != "'Adwaita'" ]
    then export TMP_GTK_THEME_USED=`gsettings get org.gnome.desktop.interface gtk-theme`
  fi
fi

gsettings set org.gnome.desktop.interface gtk-theme $SET_TO
