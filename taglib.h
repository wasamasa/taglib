#include <string.h>

#include <fileref.h>
#include <tag.h>
#include <tfile.h>
#include <tpropertymap.h>
#include <tstring.h>
#include <tstringlist.h>

TagLib::File* taglib_new(const char *filename) {
    return TagLib::FileRef::create(filename);
}

bool taglib_is_valid(TagLib::File *file) {
    return file->isValid();
}

bool taglib_save(TagLib::File *file) {
    return file->save();
}

void taglib_free(TagLib::File *file) {
    delete file;
}

TagLib::AudioProperties* taglib_audioproperties(TagLib::File *file) {
    return file->audioProperties();
}

int taglib_length(TagLib::File *file) {
    return file->audioProperties()->lengthInMilliseconds();
}

int taglib_bitrate(TagLib::File *file) {
    return file->audioProperties()->bitrate();
}

int taglib_samplerate(TagLib::File *file) {
    return file->audioProperties()->sampleRate();
}

int taglib_channels(TagLib::File *file) {
    return file->audioProperties()->channels();
}

char *taglib_title(TagLib::File *file) {
    if (file->tag()->title() == TagLib::String::null) {
        return NULL;
    } else {
        return strdup(file->tag()->title().to8Bit(true).c_str());
    }
}

char *taglib_artist(TagLib::File *file) {
    if (file->tag()->artist() == TagLib::String::null) {
        return NULL;
    } else {
        return strdup(file->tag()->artist().to8Bit(true).c_str());
    }
}

char *taglib_album(TagLib::File *file) {
    if (file->tag()->album() == TagLib::String::null) {
        return NULL;
    } else {
        return strdup(file->tag()->album().to8Bit(true).c_str());
    }
}

char *taglib_comment(TagLib::File *file) {
    if (file->tag()->comment() == TagLib::String::null) {
        return NULL;
    } else {
        return strdup(file->tag()->comment().to8Bit(true).c_str());
    }
}

char *taglib_genre(TagLib::File *file) {
    if (file->tag()->genre() == TagLib::String::null) {
        return NULL;
    } else {
        return strdup(file->tag()->genre().to8Bit(true).c_str());
    }
}

unsigned int taglib_year(TagLib::File *file) {
    return file->tag()->year();
}

unsigned int taglib_track(TagLib::File *file) {
    return file->tag()->track();
}

void taglib_title_set(TagLib::File *file, char *title) {
    if (title == NULL) {
        file->tag()->setTitle(TagLib::String::null);
    } else {
        file->tag()->setTitle(TagLib::String(title, TagLib::String::UTF8));
    }
}

void taglib_artist_set(TagLib::File *file, char *artist) {
    if (artist == NULL) {
        file->tag()->setArtist(TagLib::String::null);
    } else {
        file->tag()->setArtist(TagLib::String(artist, TagLib::String::UTF8));
    }
}

void taglib_album_set(TagLib::File *file, char *album) {
    if (album == NULL) {
        file->tag()->setAlbum(TagLib::String::null);
    } else {
        file->tag()->setAlbum(TagLib::String(album, TagLib::String::UTF8));
    }
}

void taglib_comment_set(TagLib::File *file, char *comment) {
    if (comment == NULL) {
        file->tag()->setComment(TagLib::String::null);
    } else {
        file->tag()->setComment(TagLib::String(comment, TagLib::String::UTF8));
    }
}

void taglib_genre_set(TagLib::File *file, char *genre) {
    if (genre == NULL) {
        file->tag()->setGenre(TagLib::String::null);
    } else {
        file->tag()->setGenre(TagLib::String(genre, TagLib::String::UTF8));
    }
}

void taglib_year_set(TagLib::File *file, unsigned int year) {
    file->tag()->setYear(year);
}

void taglib_track_set(TagLib::File *file, unsigned int track) {
    file->tag()->setTrack(track);
}

char** taglib_string_list_to_C(TagLib::StringList strings) {
    int i = 0, len = strings.size();
    char **ret = (char **) calloc(len + 1, sizeof(char *));
    if (ret == NULL) {
        return NULL;
    }
    for (TagLib::StringList::ConstIterator it = strings.begin();
         it != strings.end(); ++it, ++i) {
        ret[i] = strdup(it->to8Bit(true).c_str());
    }
    ret[len] = NULL;
    return ret;
}

char** taglib_property_keys_to_C(TagLib::PropertyMap props) {
    int i = 0, len = props.size();
    char **ret = (char **) calloc(len + 1, sizeof(char *));
    if (ret == NULL) {
        return NULL;
    }
    for (TagLib::PropertyMap::ConstIterator it = props.begin();
         it != props.end(); ++it, ++i) {
        ret[i] = strdup(it->first.to8Bit(true).c_str());
    }
    ret[len] = NULL;
    return ret;
}

bool taglib_raw_property_exists(TagLib::File *file, const char *key) {
    return !file->properties()[TagLib::String(key, TagLib::String::UTF8)].isEmpty();
}

char** taglib_raw_property_keys(TagLib::File *file) {
    return taglib_property_keys_to_C(file->properties());
}

char** taglib_raw_property_ref(TagLib::File *file, const char *key) {
    return taglib_string_list_to_C(file->properties()[TagLib::String(key, TagLib::String::UTF8)]);
}

void taglib_raw_property_clear(TagLib::File *file, const char *key) {
    TagLib::PropertyMap props = file->properties();
    props.erase(TagLib::String(key, TagLib::String::UTF8));
    file->setProperties(props);
}

void taglib_raw_property_set(TagLib::File *file, const char *key, C_word values) {
    TagLib::StringList list;
    while (values != C_SCHEME_END_OF_LIST) {
        C_word value = C_u_i_car(values);
        std::string s(C_c_string(value), C_unfix(C_i_string_length(value)));
        list.append(TagLib::String(s, TagLib::String::UTF8));
        values = C_u_i_cdr(values);
    }
    TagLib::PropertyMap props = file->properties();
    props[TagLib::String(key, TagLib::String::UTF8)] = list;
    file->setProperties(props);
}

char** taglib_raw_properties_set(TagLib::File *file, C_word alist) {
    TagLib::PropertyMap props;
    while (alist != C_SCHEME_END_OF_LIST) {
        C_word item = C_u_i_car(alist);
        C_word key = C_u_i_car(item);
        std::string k(C_c_string(key), C_unfix(C_i_string_length(key)));
        C_word values = C_u_i_cdr(item);
        TagLib::StringList list;
        while (values != C_SCHEME_END_OF_LIST) {
            C_word value = C_u_i_car(values);
            std::string s(C_c_string(value), C_unfix(C_i_string_length(value)));
            list.append(TagLib::String(s, TagLib::String::UTF8));
            values = C_u_i_cdr(values);
        }
        props.insert(TagLib::String(k, TagLib::String::UTF8), list);
        alist = C_u_i_cdr(alist);
    }

    return taglib_property_keys_to_C(file->setProperties(props));
}
