#!/bin/bash
ffmpeg -y -f lavfi -i anullsrc=r=48000:cl=mono -t 0.1 -c:a flac null.flac
ffmpeg -y -f lavfi -i anullsrc=r=48000:cl=mono -t 0.1 -c:a libmp3lame null.mp3
ffmpeg -y -f lavfi -i anullsrc=r=48000:cl=mono -t 0.1 -c:a libvorbis null.ogg
