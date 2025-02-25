# Load Libraries
library(stringr)
library(jsonlite)
library(tidyverse)


# Part 1
current.filename = "The Front Bottoms-Talon Of The Hawk-Au Revoir (Adios).json"

# Extract artist, album, and track

(artist = str_split_i(current.filename, "-", 1))
(album = str_split_i(current.filename, "-", 2))
(track = str_sub(str_split_i(current.filename, "-", 3), 0, 17))

# Create json

file.name = paste("EssentiaOutput/", current.filename, sep="")

json = fromJSON(file.name)
overall_loudness = json$lowlevel$loudness_ebu128$integrated
spectral_energy = json$lowlevel$spectral_energy
dissonance = json$lowlevel$dissonance
pitch_salience = json$lowlevel$pitch_salience
bpm = json$rhythm$bpm
beats_loudness= json$rhythm$beats_loudness
danceability = json$rhythm$danceability
tuning_frequency = json$tonal$tuning_frequency

# Step 2: repeat for EVERY file

files = list.files("EssentiaOutput")

artist = str_split_i(files, "-", 1)
album = str_split_i(files, "-", 2)
track = str_sub(str_split_i(files, "-", 3), end = -6)

essentia.output = tibble(.rows = 181) |>
  mutate(
    artist = artist,
    album = album,
    track = track
  )

overall.loudness = c()
spectral.energy = c()
dissonance = c()
pitch.salience = c()
bpm = c()
beats.loudness = c()
danceability = c()
tuning.frequency = c()


for(i in 1:length(files)){
  current.essentia.output = paste("EssentiaOutput/", files[i], sep="")
  current.json = fromJSON(current.essentia.output)
  overall.loudness = c(overall.loudness, current.json$lowlevel$loudness_ebu128$integrated)
  spectral.energy = c(spectral.energy, current.json$lowlevel$spectral_energy$mean)
  dissonance = c(dissonance, current.json$lowlevel$dissonance$mean)
  pitch.salience = c(pitch.salience, current.json$lowlevel$pitch_salience$mean)
  bpm = c(bpm, current.json$rhythm$bpm)
  beats.loudness= c(beats.loudness, current.json$rhythm$beats_loudness$mean)
  danceability = c(danceability, current.json$rhythm$danceability)
  tuning.frequency = c(tuning.frequency, current.json$tonal$tuning_frequency)
}

essentia.output = essentia.output |>
  mutate(
    overall.loudness = overall.loudness,
    spectral.energy = spectral.energy,
    dissonance = dissonance,
    pitch.salience = pitch.salience,
    bpm = bpm,
    beats.loudness = beats.loudness,
    danceability = danceability,
    tuning.frequency = tuning.frequency
  )
  

EssentiaModelOutput = read_csv("EssentiaModelOutput.csv")

# Create arousal and valence

EssentiaModelOutput = EssentiaModelOutput |>
  mutate(
    valence = (deam_valence + emo_valence + muse_valence)/3,
    arousal = (deam_arousal + emo_arousal + muse_arousal)/3
  )

# create aggressive, happy, party, relaxed, and sad

EssentiaModelOutput = EssentiaModelOutput |>
  mutate(
    agressive = (eff_aggressive + nn_aggressive)/2,
    happy = (eff_happy + nn_happy)/2,
    party = (eff_party + nn_party)/2,
    relaxed = (eff_relax + nn_relax)/2,
    sad = (eff_sad + nn_sad)/2
  )

# Create acoustic and electric

EssentiaModelOutput = EssentiaModelOutput |>
  mutate(
    acoustic = (eff_acoustic + nn_acoustic)/2,
    electric = (eff_electronic + nn_electronic)/2
  )

# Create instrumental

EssentiaModelOutput = EssentiaModelOutput |>
  mutate(
    instrumental = (eff_instrumental + nn_instrumental)/2
  )

# rename eff_timbre_bright

EssentiaModelOutput = EssentiaModelOutput |>
  rename(timbreBright = eff_timbre_bright)

# Only keep columns we worked on

EssentiaModelOutput = EssentiaModelOutput |>
  select(artist, album, track, valence, arousal, agressive, happy, party, relaxed,
         sad, acoustic, electric, instrumental)

# Load LIWCOutput.csv



LIWCOutput = read.csv("LIWCOutput/LIWCOutput.csv")

final.df = essentia.output |>
  right_join(EssentiaModelOutput, by = c("artist", "album", "track"), suffix = c("",""))


final.df = final.df |>
  right_join(LIWCOutput, by = c("artist", "album", "track"), suffix = c("",""))

# change name of function.

final.df = final.df |>
  rename(funct = function.)

# Write csv file

write_csv(final.df |> filter(track != "Allentown"), file = "trainingdata.csv")
write_csv(final.df |> filter(track == "Allentown"), file = "testingdata.csv")

# Create Plots

ggplot(data = final.df)+
  geom_boxplot(aes(x = artist,
                   y = sad))

ggplot(data = final.df)+
  geom_boxplot(aes(x = artist,
                   y = Tone))

ggplot(data = final.df)+
  geom_boxplot(aes(x = artist,
                   y = agressive))
