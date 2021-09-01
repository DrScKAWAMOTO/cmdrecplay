cmake_minimum_required(VERSION 3.0.0)

project(cmdrecplay)
set(CMDRECPLAY_VERSION "2.0.pre2")

set(CPACK_PACKAGE_NAME "cmdrecplay")
set(CPACK_PACKAGE_CONTACT "Dr.Sc.KAWAMOTO,Takuji")
set(CPACK_PACKAGE_VENDOR ${CPACK_PACKAGE_CONTACT})
set(CPACK_PACKAGE_VERSION ${CMDRECPLAY_VERSION})
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "cmdrec/cmdplay")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
set(CPACK_PACKAGING_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX})
set(CPACK_RPM_PACKAGE_RELEASE "1%{?dist}")
set(CPACK_RPM_PACKAGE_LICENSE "GPLv3")
set(CPACK_RPM_PACKAGE_GROUP "Development/Tools")
set(CPACK_RPM_PACKAGE_URL "http://kawamoto.mydns.jp/")
set(CPACK_RPM_PACKAGE_DESCRIPTION "Cmdrecplay is a tool for C/C++ checker of emacs flycheck, for auto-complete-clang-async, for company, for reporting valid lines by `#if #endif directives`, with C/C++ language source codes.")
include(CPack)

find_package(PkgConfig)
pkg_check_modules(LIBCONFIG REQUIRED libconfig>=1.4)
pkg_check_modules(ONIGURUMA REQUIRED oniguruma>=6.9.7)
pkg_check_modules(SQLITE3 REQUIRED sqlite3>=3.32.3)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR})
find_package(C99 REQUIRED)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${C99_C_FLAGS}")

if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release")
endif()

include(GNUInstallDirs)
install(FILES LICENSE README.md ChangeLog.md
  DESTINATION ${CMAKE_INSTALL_DOCDIR})

set(DEFAULT_CMDREC_CONFIG_FILE ${CMAKE_INSTALL_FULL_SYSCONFDIR}/cmdrec.conf)
set(DEFAULT_CMDPLAY_CONFIG_FILE ${CMAKE_INSTALL_FULL_SYSCONFDIR}/cmdplay.conf)
set(INSTALL_FULL_SBINDIR ${CMAKE_INSTALL_FULL_SBINDIR})

add_subdirectory(src)
add_subdirectory(emacs)
#add_subdirectory(doc)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
  message(STATUS "*** dump start cmake variables ***")
  get_cmake_property(_variableNames VARIABLES)
  foreach(_variableName ${_variableNames})
          message(STATUS "${_variableName}=${${_variableName}}")
  endforeach()
  message(STATUS "*** dump end ***")
endif()
