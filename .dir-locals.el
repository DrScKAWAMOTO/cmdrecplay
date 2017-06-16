;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (projectile-project-compilation-cmd . "(rm -rf build; mkdir build; cd build; cmdrec -s ~/cmdskin -- cmake ..; cmdrec -s ~/cmdskin -- gmake)")
  (projectile-project-compilation-dir . ".")))

