
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config.h.in ${CMAKE_CURRENT_BINARY_DIR}/config.h)
include_directories(${CMAKE_CURRENT_BINARY_DIR})


add_definitions(-D_GNU_SOURCE)


link_directories(${SQLITE3_LIBRARY_DIRS} ${ONIGURUMA_LIBRARY_DIRS})
add_executable(cmdrec
    ConfigFile.c
    Pattern.c
    RecPattern.c
    ParameterSet.c
    Argv.c
    Regexp.c
    cmdrec.c)
add_executable(cmdplay
    ConfigFile.c
    Pattern.c
    PlayPatterns.c
    Database.c
    PlayDatabase.c
    SrcDatabase.c
    ParameterSet.c
    Argv.c
    Regexp.c
    cmdplay.c)
add_executable(cmdsrc
    Database.c
    cmdsrc.c)
add_executable(cmdskin
    ConfigFile.c
    Pattern.c
    RecPattern.c
    Database.c
    RecDatabase.c
    ParameterSet.c
    Argv.c
    Regexp.c
    cmdskin.c)
add_executable(ifendif
    AtExitUnlink.cc
    Expanded.cc
    FillTheSkips.cc
    LineBuffer.cc
    RegionDetector.cc
    Source.cc
    ifendif.cc)
add_executable(srcinc
    AtExitUnlink.cc
    Included.cc
    FillTheSkips.cc
    LineBuffer.cc
    Database.c
    SrcDatabase.c
    srcinc.cc)
include_directories(${SQLITE3_INCLUDE_DIRS} ${ONIGURUMA_INCLUDE_DIRS} ../src)
target_link_libraries(cmdrec ${SQLITE3_LIBRARIES} ${ONIGURUMA_LIBRARIES})
target_link_libraries(cmdplay ${SQLITE3_LIBRARIES} ${ONIGURUMA_LIBRARIES})
target_link_libraries(cmdsrc ${SQLITE3_LIBRARIES})
target_link_libraries(cmdskin ${SQLITE3_LIBRARIES} ${ONIGURUMA_LIBRARIES})
target_link_libraries(srcinc ${SQLITE3_LIBRARIES})
set_target_properties(cmdrec PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)
set_target_properties(cmdplay PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)
set_target_properties(cmdsrc PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)
set_target_properties(cmdskin PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)
set_target_properties(ifendif PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)
set_target_properties(srcinc PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)

include(GNUInstallDirs)
install(TARGETS cmdrec cmdplay cmdsrc ifendif srcinc
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})
install(TARGETS cmdskin
    RUNTIME DESTINATION ${CMAKE_INSTALL_SBINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})
install(FILES cmdrec.conf cmdplay.conf
    DESTINATION ${CMAKE_INSTALL_SYSCONFDIR})
