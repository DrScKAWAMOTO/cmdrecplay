
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config.h.in ${CMAKE_CURRENT_BINARY_DIR}/config.h)
include_directories(${CMAKE_CURRENT_BINARY_DIR})


add_definitions(-D_GNU_SOURCE)


link_directories(${LIBCONFIG_LIBRARY_DIRS})
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
    ParameterSet.c
    Argv.c
    Regexp.c
    cmdplay.c)
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
include_directories(${LIBCONFIG_INCLUDE_DIRS})
target_link_libraries(cmdrec ${LIBCONFIG_LIBRARIES} onig sqlite3)
target_link_libraries(cmdplay ${LIBCONFIG_LIBRARIES} onig sqlite3)
target_link_libraries(cmdskin ${LIBCONFIG_LIBRARIES} onig sqlite3)

include(GNUInstallDirs)
install(TARGETS cmdrec cmdplay
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
install(TARGETS cmdskin
    RUNTIME DESTINATION ${CMAKE_INSTALL_SBINDIR})
install(FILES cmdrec.conf cmdplay.conf
    DESTINATION ${CMAKE_INSTALL_SYSCONFDIR})
