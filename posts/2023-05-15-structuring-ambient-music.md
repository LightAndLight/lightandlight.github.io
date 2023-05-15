---
title: Structuring Ambient Music
permalink: /structuring-ambient-music
---

From 2012 to 2020 I mostly listened to metal music. So when I started making music in 2018, I naturally wrote rock and metal.

Composing is hard, and I knew that I would only get better if I made a lot of music. I decided that the best way to do that
would be to make fully-finished songs. Over two years I wrote 12 songs with this in mind, and I definitely improved
([2019](https://soundcloud.com/lightandlight/sets/2019-1), [2020](https://soundcloud.com/lightandlight/sets/2020-1)).

For the past couple of years I've mostly been listening to electronic music, movie and video game soundtracks, and ambient music.
I'm not very interested in creating metal music right now; I'd really like to make some music in the genres I currently listen to.
I play around with synths and sound design now and then, but haven't made any songs so far. Comparing my current experience to
when I was writing metal, I think one of my main difficulties in writing more atmospheric music is a(n apparent) lack of structure
in the genre.

When I wrote metal songs, I had a [conventional song structure](https://en.wikipedia.org/wiki/Song_structure) to follow, often something
like: intro, verse, chorus, verse, chorus, bridge, chorus, outro. This structure broke songwriting into smaller pieces and gave me a measure
of how "done" I was.

When I think about ambient / environmental / soundtrack music I don't have any clear sense of structure. A metal song is
like a journey, usually through some familiar landmarks, and will eventually conclude by arriving at a destination. The kinds
of electronic music I'd like to make seems a lot more nebulous. A lot of video game music isn't supposed to recognisably end,
so that it can fade in and out, matching your environment as you move about. A metal song, on the other hand, would overstay its
welcome if it were put on repeat or stretched out, like you'd get tired if you were forced to repeatedly go on a hike or tour.
If a metal song takes you on a journey, then maybe I can say that an ambient song "puts you in an environment".

So how do I actually *finish* some ambient music? A metal song is complete when I've charted a course through
various landmarks and arrived at my destination. Each section I write, each transition, represents progress towards
finishing. If an ambient song constructs an environment, how do I know when I've done enough?

It seems like ambient music isn't as linear as metal songs.

I suspect that ambient music is not as nebulous as it seems. I'm going to analyse some music from video games I like to
see if there's anything I can learn here.

## Deus Ex: Human Revolution - Detroit Ambient

The first piece I'll analyse is "Detroit Ambient" from [Deus Ex: Human Revolution](https://en.wikipedia.org/wiki/Deus_Ex:_Human_Revolution).
The two sources I'm using for this analysis are:

* ["Detroit City Ambient (Part 1)" (Spotify](https://open.spotify.com/track/0XUBDdyDYXnbozXj5Mtkrk?si=b89814220655439e)
* ["Deus Ex: Human Revolution - Detroit Police Station Streets" (1 Hour of Music) (YouTube)](https://www.youtube.com/watch?v=ea0UrsSqp10)

  I'm using this because it might be recorded from actual gameplay.

The first thing I notice that that ambient doesn't mean "without melody". Immediately I hear the iconic synth melody. This melody plays
in variation at:

* [00:00](https://www.youtube.com/watch?v=ea0UrsSqp10&t=0s)
* [00:08](https://www.youtube.com/watch?v=ea0UrsSqp10&t=8s)
* [00:15](https://www.youtube.com/watch?v=ea0UrsSqp10&t=15s)
* [00:23](https://www.youtube.com/watch?v=ea0UrsSqp10&t=23s)
* [00:30](https://www.youtube.com/watch?v=ea0UrsSqp10&t=30s)
* [00:45](https://www.youtube.com/watch?v=ea0UrsSqp10&t=45s)
* [01:51](https://www.youtube.com/watch?v=ea0UrsSqp10&t=111s)
* [01:58](https://www.youtube.com/watch?v=ea0UrsSqp10&t=118s)
* [02:06](https://www.youtube.com/watch?v=ea0UrsSqp10&t=126s)
* [02:13](https://www.youtube.com/watch?v=ea0UrsSqp10&t=133s)
* [02:21](https://www.youtube.com/watch?v=ea0UrsSqp10&t=141s)
* [02:35](https://www.youtube.com/watch?v=ea0UrsSqp10&t=155s)
* [03:41](https://www.youtube.com/watch?v=ea0UrsSqp10&t=221s)

The 1 hour mix is actually a seamless ~1min 50s loop.

This also means that ambient isn't necessarily arhythmic. This track runs at around 65bpm, according to the melody.

---

Aside: I was curious what music actually comes with the game. I own the game, so I reinstalled it and extracted the sound files.

Guides:
* https://forums.anandtech.com/threads/is-there-a-way-to-extract-the-music-from-deus-ex-human-revolution.2281210/post-34198807
* https://steamcommunity.com/sharedfiles/filedetails/?id=2421822010

Tools:
* Compiled extractor tools: <https://www.moddb.com/games/deus-ex-3/downloads/gibbeds-deus-ex-hr-tools>
* `foobar2000`: https://www.foobar2000.org/download
* `vgmstream` plugin for `foobar2000`: https://www.foobar2000.org/components/view/foo_input_vgmstream

Process:
* Use Gibbed's unpacker on the game's `bigfile.000`
* Run Gibbed's demuxer on every `.mul` file, producing `.fsb` files
* Move `.fsb` files into a single directory
* Open the directory in `foobar2000` and convert all items to `.wav`

All the music's in mono, which is weird. Maybe a bug in the tooling? Anyway, it's good enough for analysis. The track I'm studying
is called `det1_city_police_music` and is 3m52s long.

---

Why does this work as ambient music? And can I learn anything that will help me finish a song? I'll
be happy if I can reverse-engineer some goals for this style of composition.

### Harmony

* Lots of time on the root (B)
* Ventures up to minor 3rd (D)
* And sometimes includes the minor 6th (G)

My general impression is very little harmonic motion. Jump away from the root, and come back. I wouldn't call it
a chord progression. There's a main harmony with short movements away. That said, the changes in harmony still
sound meaningful. When we get the first G major chords at [00:23](https://www.youtube.com/watch?v=ea0UrsSqp10&t=23s) and
[00:38](https://www.youtube.com/watch?v=ea0UrsSqp10&t=38s), it sounds powerful, while also wanting to be resolved to
the root. I think this influences my sense of the song "not heading anywhere particular", which is important for its
ambient nature. I do find myself anticipating the G major chord motion.

### Change

Even though there's very little harmonic motion, basically no chord progression, it doesn't feel like 4 minutes of
sameness. Like, it's not just a single note played ad nauseum. The music is changing, moving through ideas, without
necessarily going somewhere specific.

In the parts that aren't driven by the synth melody
([00:50](https://www.youtube.com/watch?v=ea0UrsSqp10&t=50s) to [01:50](https://www.youtube.com/watch?v=ea0UrsSqp10&t=1m50s))
there's still a lot happing to provide a sense of motion and change. Every second or so there's a subtle sound to break the
potential monotony of the main chords. Around [00:50](https://www.youtube.com/watch?v=ea0UrsSqp10&t=50s), a bass pluck interjects.
At [00:52](https://www.youtube.com/watch?v=ea0UrsSqp10&t=52s), a light tom hit and something "clicky" with a delay. Another pluck.
At [00:59](https://www.youtube.com/watch?v=ea0UrsSqp10&t=59s), an eery bowed metal sound that then leads in to a mournful descending
melody (E D C#) that hasn't yet been played. At [01:15](https://www.youtube.com/watch?v=ea0UrsSqp10&t=75s) the G major chord plays,
but it's different to the previous ones because a new bowed metal melody (sounds a bit like G E F#) comes in over the top. When the
root note comes back, some lower-pitched bowed metal plays [01:23](https://www.youtube.com/watch?v=ea0UrsSqp10&t=83s) to add variation.

There are some common instruments playing chords that are sustained and repeated, but they're played in a changing context:
alongside new textural sounds, punctuated by a different instrument, or underneath a new melodic flourish.

Maybe one goal I can keep in mind when composing an ambient song is: variety. Is this "song" just a single chord on a single
intrument? It could probably do with more variation. I have to balance this against being too busy or incoherent. Using lots
of extra sounds could provide too much stimulation, and there might be such a huge variety as to seem randomly chosen and inserted.

Variety is something I have trouble with in general. Metal music is built around guitars
and drums, which constrains the composition. In the kinds of ambient / environmental / soundtrack music I'm thinking about, the
sound possibilities are endless. So if I should add interesting sounds to my music, how do I decide what to add? I'll address this
in a different post.
