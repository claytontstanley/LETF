# this file generated when environment is closed
# refresh . to make sure sizes are right

wm deiconify .
update
wm withdraw .
if {[winfo screenwidth .] != 1440 || [winfo screenheight .] != 900 || [lindex [wm maxsize .] 0] != 1386 || [lindex [wm maxsize .] 1] != 856} {
  set size_mismatch 1
} else {
  set size_mismatch 0
}

if $size_mismatch {
  set reset_window_sizes [tk_messageBox -icon warning -title "Screen resolution changed" -type yesno \
                                         -message "The screen resolution is not the same as it was the last time the Environment was used.  Should the window positions reset to the defaults?"]
} else { set reset_window_sizes 0}
if {$reset_window_sizes != "yes"} {
  set window_config(.control_panel) 170x710+1260+125
  set changed_window_list(.control_panel) 1
  set window_config(.copyright) 1x1+520+217
  set changed_window_list(.copyright) 1
}
