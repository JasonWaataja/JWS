# JWS
A wallpaper setter for minimalist window managers

## Synopsis
JWS is a small project for setting up wallpapers on minimalist window managers
or any desktop environment that supports setting wallpapers with feh. There's a
command line utility called jws with acts as the daemon for setting the
wallpaper and is supposed to be started with the WM or DE with the command `jws`
or `jws -c /path/to/config/file`.
[JWS-Config](https://github.com/JasonWaataja/JWS-Config) is a graphical tool for
creating the config file that is used by JWS. It is highly recommended that you
check it out and use it to configure JWS. JWS uses feh as a backend to set
wallpapers and its main benefit is ease of configuration and management (via
JWS-Config).

## Installation
JWS can be installed on Arch Linux from the AUR with the jws-git package. Try
jws-config-git for the configuration tool.

JWS is written in C and built using autotools. It also depends on GLib as well
as gettext. On Arch Linux, the dependencies can be installed with the packages
`base-devel`, `glib2`, and `feh`. On Ubuntu, try `build-essential`, `feh`,
`autotools-dev`, `autoconf` `autopoint`, and `libglib2.0-dev`. I don't know that
it's those exactly so correct me if I'm wrong.

To compile and install the program, cd into the directory where you cloned the
repository. Run `./autogen.sh`, `./configure`, `make`, and `make install`. You
might need sudo for the last one.

## Usage
The programs can be run with jws and jws-config. The jws program will look for a
configuration file at $HOME/.jws and then ~/.config/jws or wherever
$XDG_CONFIG_HOME is set to. It will then read the file and change the wallpaper
if it's single or act as a daemon if there are multiple and will stick around
indefinitely.

Both programs work by having a list of files which are either image files or
directories. If it's a image file, it will be set as a wallpaper and if it's a
directory, then it will be recursively searched for all of its files and set
them. There's currently no way to exclude some portion of a subdirectory because
then the configuration would either have to specify what files to exlude or the
list of files to include which is not what I want. I want to be able to add a
wallpaper to a directory in the top level and have it appear without additional
configuration. If you want to only use some files from a subdirectory, then
specify them as toplevel images.

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
listed in the files section that it will display all the images continuously and
repeat until the program is stopped.

A line starting with `single-image` is the opposite, it will make the program
display one image and stop.

A line starting with `randomize-order` will shuffle the images to display
instead of the order they are presented.

A line starting with `in-order` will display them in order.

A line starting with `time` will set the time in between rotating images if
`rotate-image` is on. The format is \[XXh\]\[YYm\]\[ZZs\]. You can combine these
in any combination that you want. Leaving out a unit is interpreted as seconds.
For example, "time 3m20s" will rotate the image every three minutes and 20
seconds and "time 120" will rotate the image every two minutes.

#### Files Section
After a line with the text `files`, any lines with text on them are treated as
files or directories to be added. Try to use absolute paths. An regular file
will be added to the list of images to display and a directory will be displayed
recursively with the images inside of it.

#### Example Configuration File
My configuration file looks like this:
```
rotate-image
randomize-order
time 10s

files
/home/jason/Dropbox/Backgrounds
```

#### Command Line Arguments
The jws executable also accepts command line arguments that supersede the config
file. Check the soon to exist man page for a list of available options.
