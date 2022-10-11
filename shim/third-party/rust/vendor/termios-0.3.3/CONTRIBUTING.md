# Contributing

Termios is a pervasive API on Unix-like systems. One person can't possibly be an expert is all of
the platforms that `termios-rs` may want to target. This document provides guidelines for how we can
work together as complete strangers while maintaining a high standard for quality.

## General Guidelines

* Above all, don't be a jerk. You won't get what you want that way.

* Please try to keep discussion focused on improving this project. Don't use the issue tracker to
  promote other projects. A brief mention of another project as it relates to the development of
  this project is okay. This also isn't the place for political activism.

* Remember that nobody is paid to work on this project. There are no contractual obligations, and no
  promises have been made. Maintainers have friends, family, kids, hobbies, and responsibilities
  outside of this project. Please calibrate your expectations accordingly.

* Maintainers have the final say. The maintainers are not your co-workers and they don't work for
  you. Please calibrate your expectations accordingly.

* Avoid "+1" or similar comments. Try to find a more constructive way to move the project forward. A
  comment with how you've tested a patch and your results adds more value than "+1" while serving
  the same purpose. See "[Reviewing Patches](#reviewing-patches)" below for ideas of how you can
  help.

* If you want to see something get done, the best way is to roll up your sleeves and do it. Read
  below for details on how you can help.

## Moderating

Some people just don't know how to behave on the internet. Unfortunately, this is one of the leading
causes for maintainers to walk away from their projects. That makes moderating user behavior one of
most important things that any mentally-stable adult can do to contribute to an open source project.
Being a moderator doesn't require being blessed with a special status. Anyone can help.

If you see anyone being rude, acting with a sense of entitlement, putting down others or their work,
arguing in bad faith, or in any other way making the project an unpleasant place to spend one's
time, please call out the bad behavior. As a maintainer, it helps to see that we're not on our own
when dealing with bad behavior.

This includes enforcing theses contributing guidelines. If you see patches or issues that don't
appear to be based on these guidelines, please ask the submitter to provide the missing details.

If you'd prefer to email me privately about someone's behavior, please do so. My email can be found
in public profiles or the project's commit history.

## Adding Support for New Platforms

The currently supported platforms are listed in the README. If an operating system and architecture
aren't listed there, it means that nobody has confirmed that `termios-rs` works on that platform.
The following sections describe different ways that one can help add support for a new platform.
The guidelines in this section are meant to increase the chances that a patch can land smoothly the
first time that a maintainer reviews it.

### Sending Patches

Patches should be based on reading the C header file `termios.h`. This is often located at
`/usr/include/termios.h` or `/usr/include/sys/termios.h`. You may have to install a package to get
the header files.

IMPORTANT: Please do not count on a platform being so similar to another platform that you can just
copy the definitions. I'm pretty sure every patch that has been submitted that way has proven the
assumption to be wrong. That just wastes everyone's time. Don't submit patches without testing them.

To make a patch for a new platform, you'll need to setup the target platform with a working Rust
compiler and `termios.h` header file. Sometimes a working C compiler can help, too. Please take note
of the steps you use to setup your platform so that you can provide testing guidance when submitting
your patch. See "[Testing Guidance](#testing-guidance)," for details.

1.  Start by creating a new file in `src/os/` for the target platform. To keep things consistent,
    pick a name that matches the value of Rust's `target_os` configuration variable, e.g.,
    `src/os/freebsd.rs`.

2.  Adjust the `use` and `mod` items in `src/os/mod.rs`.

3.  Add an example to the doctest under "OS-Specific Extensions" in `src/lib.rs`. `cargo test` won't
    pass without it.

3.  Read through the `termios.h` header file and transliterate it to Rust. Someone reading through
    the  Rust file and C header file side-by-side should be able to easily verify that they produce
    the same values. That means:

    * Values in the Rust file should be defined in the same radix (decimal, octal, hexidecimal) as
      the C header file.

    * Definitions should appear roughly in the same order that they appear in the C header file.
      Please try to keep them grouped similar to other platforms. That means keeping `c_cc` character
      indexes together, `c_iflag` bits together, `c_oflag` bits together, etc. But within each of
      those groups, match the order of the C header file.

    One exception to transliterating the header file is to expand macros to their literal value.
    For example,

    ```c
    #define _TIOC 'T'
    #define TCSANOW (_TIOC|14)
    ```

    should be reduced to

    ```rust
    pub const TCSANOW: c_int = 0x005E;
    ```

    In this case, simplicity is the overriding concern. This is where having a C compiler on the
    target platform can help. To avoid error in computing the literal value, you can let a C
    compiler do it for you:

    ```c
    #include <stdio.h>
    #include <stdlib.h>
    #include <termios.h>

    int main(void)
    {
        printf("TCSANOW = 0x%04X\n", TCSANOW);
        return EXIT_SUCCESS;
    }
    ```

4.  Check which of the functions in `src/ffi.rs` the target platform implements (look for them in
    `termios.h`). If the target platform doesn't implement one or more of them, adjust the `#[cfg]`
    items in `src/ffi.rs` to use the fallback imlementations. (`cfmakeraw()` and `cfsetspeed()` are
    technically not part of the [POSIX standard for termios][posix]. Many operating systems provide
    them, but some do not.)

5.  Make sure `cargo build` and `cargo test` for `termios-rs` both pass on the target platform.

6.  Optionally, test some projects that depend on `termios-rs`. Some popular projects that depend on
    `termios-rs` include:

    * [`serial-rs`](https://github.com/dcuddeback/serial-rs)

    * [`bat`](https://github.com/sharkdp/bat)

When submitting a patch, please provide testing guidance. See the section, "[Testing
Guidance](#testing-guidance)," for details.

### Reviewing Patches

The best way to help land an existing patch is to review it. If you review a patch, please leave a
review comment on its submission so that maintainers can see how many eyes have looked at a patch.
If you don't comment, we won't know. And commenting on a patch with what you've done to review and
test it is much more helpful than "+1" comments.

See the section, "[Testing](#testing)," for details on how to test a patch. If you tested something
that others hadn't, such as a different version of an operating system or a different reverse
dependency, please comment with details of what you tested and the results.

Check the patch for conformance to the guidelines described in "[Sending
Patches](#sending-patches)." Feel free to add line comments where you spot anything that looks
incorrect. Whether or not you spot any errors, please leave a review comment so that maintainers
know how much peer-review a patch has received.

Make sure that the patch was submitted with sufficient testing guidance. See the section, "[Testing
Guidance](#testing-guidance)," for details. If the guidance seems incomplete, ask the submitter to
provide additional details or add the relevant details yourself if you can. If you have an
alternative way of testing, feel free to provide those details according to the "[Testing
Guidance](#testing-guidance)" section.

### Testing

If you're interested in a different architecture of an existing operating system, it may be just a
matter of testing the new architecture. Or if you see a patch for a platform you're interested in,
you can help test that patch, too.

To test platform support, you'll need to have a working Rust compiler on the target platform, and
you'll need the C header files for `termios`. A working C compiler can help, too.

1.  Find the Rust definitions for the platform you're testing. This will be in
    `src/os/{platform}.rs`, e.g., `src/os/freebsd.rs` for FreeBSD.

2.  Find the C header file for your platform. This is often located at `/usr/include/termios.h` or
    `/usr/include/sys/termios.h`. On some platforms, you may have to install a package to get the
    header files. (If so, please report which package.)

3.  Verify that the definitions in the Rust code match the definitions from the C header file. If
    reviewing a new patch submission, verify that the values are defined in the same radix as the C
    header file and are defined in roughly the same order. If the submitter followed the guidelines
    in "[Sending Patches](#sending-patches)," this step should go smoothly.

4.  Verify that `cargo build` and `cargo test` for `termios-rs` both succeed on the target platform.

5.  Optionally, test some projects that depend on `termios-rs`. Some popular projects that depend on
    `termios-rs` include:

    * [`serial-rs`](https://github.com/dcuddeback/serial-rs)

    * [`bat`](https://github.com/sharkdp/bat)

Please report back on your findings. Knowing that a platform has had several sets of eyes looking at
it is helpful.

If you tested a new architecture for an already-supported operating system and everything appears
correct, please submit a patch to add the architecture to the list of supported platforms in the
README. When submitting such a patch, please describe what you tested, including any reverse
dependencies, and provide any testing guidance you can for the maintainers. See the section,
"[Testing Guidance](#testing-guidance)," for details.

### Testing Guidance

Before landing a patch, a maintainer will need to verify the patch on the target platform. That
means a maintainer will need to setup a virtual machine running the target platform, setup SSH
access, and install a working Rust compiler, `termios.h` header files, and possibly a C compiler.
Any instructions you can provide to help a maintainer or other testers through those steps will help
land the patch quicker. Try to be as complete and precise as possible, including full commands where
appropriate.

Some helpful tips could include:

  * How to obtain installation media for the target platform. Some platforms are made up of many
    distributions. Some less popular platforms may not have official Rust support or may have
    recently gained Rust support in their latest nightly snapshot. If this matters, please point to
    the release channel that provides the greatest chance of success of getting things working. At
    the very least, just tell us what you used (which distribution, version, and release channel, if
    applicable).

  * Any special steps to setup a virtual machine. How should the virtual hardware be setup? For less
    popular architectures, providing specific commands to use with QEMU or libvirt would be ideal.
    If you only have instructions for other hypervisors, go ahead and provide them.

  * How to setup and configure SSH access. What packages need to be installed? Is there a
    configuration file that needs to be edited?

  * How to install Rust and Cargo. Does `rustup` work on the target platform? Is Rust in the
    platform's package manager? What's the package name? Does it need to be installed from an
    unofficial channel?

  * How to install the termios header files and a C compiler. Usually these are just packages that
    needs to be installed. Which packages?

The easier it is for a maintainer to verify a patch, the easier it will be for a maintainer to land
the patch. :)

### Requesting Support for New Platforms

Platforms that have been tested are listed in the README. Any platform that implements the termios
API is fair game to add to `termios-rs`, so there is implicitly an open ticket for any platform not
listed in the README. If you're interested in a platform that isn't listed in the README, you may
open a ticket to request support, but that won't change much. Someone will need to roll up their
sleeves and do the work to support it. At most, a ticket provides a place to coordinate who's
working on support for a platform.

## Other Requests

There may occasionally be a small enhancement worth considering, like adding a trait implementation
or adding to the documentation. Go ahead and open tickets or send patches for small enhancements.

Most other requests are probably out of scope. `termios-rs` aims to be a very stable, low-churn
library with wide platform support. There's little value in constantly refactoring it, adding
dependencies, or debating design patterns.

This also isn't the place to ask for help with general programming questions or how to use the
termios API. The scope of this project is to provide the Rust bindings for the termios API. The
termios API itself is standardized by POSIX and provided by your operating system. The [POSIX
standard][posix], your operating system's manual pages, and internet searches are your best
resources for termios-specific help.

[posix]: https://pubs.opengroup.org/onlinepubs/009695399/basedefs/termios.h.html
