
(define (window-add Window Widget)
  (if (provided? 'gtk4)
      (gtk_window_set_child (GTK_WINDOW Window) (GTK_WIDGET Widget))
      (gtk_container_add (GTK_CONTAINER Window) Widget)))


