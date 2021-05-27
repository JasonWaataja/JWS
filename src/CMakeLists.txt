add_executable(
    jws
    main.c
    jwsinfo.c
    jwssetter.c
    jwstime.c
    jwsapplication.c
    )

target_include_directories(jws PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

target_compile_options(jws PRIVATE -Werror -Wall -Wextra)

target_include_directories(jws PRIVATE ${GLIB_INCLUDE_DIRS})
target_link_libraries(jws PRIVATE ${GLIB_LIBRARY_DIRS})
target_link_libraries(jws PRIVATE ${GLIB_LIBRARIES})

target_include_directories(jws PRIVATE ${GIO_INCLUDE_DIRS})
target_link_libraries(jws PRIVATE ${GIO_LIBRARY_DIRS})
target_link_libraries(jws PRIVATE ${GIO_LIBRARIES})

install (TARGETS jws DESTINATION bin)
