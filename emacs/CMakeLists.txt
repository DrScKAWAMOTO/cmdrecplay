
include(GNUInstallDirs)
install(FILES cmdplay.el
    DESTINATION ${CMAKE_INSTALL_DATADIR}/emacs/site-lisp)
install(FILES cmdplay-flycheck.el
    DESTINATION ${CMAKE_INSTALL_DATADIR}/emacs/site-lisp)
install(FILES cmdplay-clang-async.el
    DESTINATION ${CMAKE_INSTALL_DATADIR}/emacs/site-lisp)
install(FILES cmdplay-ifendif.el
    DESTINATION ${CMAKE_INSTALL_DATADIR}/emacs/site-lisp)
