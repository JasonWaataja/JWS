# JWS
A small project for setting up wallpapers on minimal window managers or any
desktop environment that supports setting wallpapers with feh. There's a
command line utility called jws with acts as the daemon for setting the
wallpaper and is supposed to be started with the WM or DE with the command
`jws` or `jws -c /path/to/config/file`. JWS-Config is a graphical tool for
creating the config file that is used by JWS.

## Installation
JWS and JWS-Config can and must be installed separately. They both use
autotools so the procedure will be the same.

### Dependencies
Both programs require are written in C. You'll need GLib, pkg-config,
autotools, and feh as well as GTK+ 3 for JWS-Config. At least for now, gettext
is a requirenment for JWS and will be in the future for JWS-Config.

On Arch Linux, these can be installed with just `feh`, `base-devel`, `glib2`,
and `gtk3`.

On Ubuntu, it's `feh`, `build-essential`, `autotools-dev`, `autoconf`,
`autopoint`, `libglib2.0-dev`, `libgtk-3-dev`. Correct me if I'm wrong there.

### Compiling
For either program, cd into the directory with `cd jws` or `cd jws-confg`. Run
`./autogen.sh`, `./configure`, `make`, and if installation to the system is
desired: `sudo make install`.


## Usage
The programs can be run with jws and jws-config. The jws program will look for
a configuration file at $HOME/.jws and then ~/.config/jws or wherever
$XDG_CONFIG_HOME is set to. It will then read the file and change the
wallpaper if it's single or act as a daemon if there are multiple and will
stick around indefinitely.

The jws-config program tries to read $HOME/.jws and loads it. You can open
open another configuration file but hitting apply or Save in the file menu
will save it to $HOME/.jws. Saving to another location will require using Save
as.

Both programs work by having a list of files which are either image files or
directories. If it's a image file, it will be set as a wallpaper and if it's
a directory, then it will be recursively searched for all of its files and set
them. There's currently no way to exclude some portion of a subdirectory
becuase then the configuration would either have to specify what files to
exlude or the list of files to include which is not what I want. I want to be
able to add a wallpaper to a directory in the top level and have it appear
without additional configuration. If you want to only use some files from a
subdirectory, then specify them as toplevel images.

To use JWS-Config, open the program and you'll be greeted by t gui tool. This
can be used to add files, directories, sort items, and remove them. It is
useful because it allows you to see the images in the tool and it also allows
you to open them by double clicking on an image which opens the image viewer.

The image viewer can display the image at a small scaled size or the native
size. You can go to the next and previous images by clicking on the image with
the primary and middle buttons respectively.

### Configuration
The recommended way to configure JWS is through running JWS-Config. All that
does is write to ~/.jws so writing the file yourself is completely fine.

The configuration file has two parts: the settings section and files section.
Parsing of the files sections will be started by a single line with the word
"files" on it. No settings are necessary as they all have defaults but you
should set them to what you want and that also means you don't have to install
JWS-Config at all.

#### Options
A line starting with `rotate-image` will mean that if more than one image is
listed in the files section that it will display all the images continuously
and repeat until the program is stopped.

A line starting with `single-image` is the opposite, it will make the program
display one image and stop.

A line starting with `randomize-order` will shuffle the images to display
instead of the order they are presented.

A line starting with `in-order` will display them in order.

A line starting with `time` will set the time in between rotating images if
`rotate-image` is on. It is in seconds.

#### Files section
After a line with the text `files`, any lines with text on them are treated as
files or directories to be added. Try to use absolute paths. An regular file
will be added to the list of images to display and a directory will be
displayed recursively with the images inside of it.
