# Audio-File-Hashing-for-TheAudioDB.com
This sample application uses a proposed file hashing algorithm designed for future use by TheAudioDB.com

The algorithm itself is based on a modified version of the OpenSubtitles.org code:
http://www.yanniel.info/2012/01/open-subtitles-api-in-delphi.html

Unlike the OpenSubtitles.org hash, in this case, hash offset position within the file is determined by the file size to support smaller file sizes, while allowing larger TAG data (embedded images) to be changed without affecting both hashes (unless the embedded image changes the file size from under 2048KiB to over 2048KiB).
