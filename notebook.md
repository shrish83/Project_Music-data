
## 1. Introduction
<p>How do musicians choose the chords they use in their songs? Do guitarists, pianists, and singers gravitate towards different kinds of harmony?</p>
<p>We can uncover trends in the kinds of chord progressions used by popular artists by analyzing the harmonic data provided in the <a href="http://ddmal.music.mcgill.ca/research/billboard">McGill Billboard Dataset</a>. This dataset includes professionally tagged chords for several hundred pop/rock songs representative of singles that made the Billboard Hot 100 list between 1958 and 1991. Using the data-wrangling tools available in the <code>dplyr</code> package, and the visualization tools available in the <code>ggplot2</code> package, we can explore the most common chords and chord progressions in these songs, and contrast the harmonies of some guitar-led and piano-led artists to see where the "affordances" of those instruments may affect the chord choices artists make.</p>


```R
# Loading individual Tidyverse packages
# .... YOUR CODE FOR TASK 1 ....
library(dplyr)
library(readr)
library(ggplot2)

# Reading in the McGill Billboard chord data
bb <- read.csv('datasets/bb_chords.csv')

# Taking a look at the first rows in bb
# .... YOUR CODE FOR TASK 1 ....
head(bb)
```


<table>
<caption>A data.frame: 6 x 9</caption>
<thead>
	<tr><th scope=col>year</th><th scope=col>chord</th><th scope=col>root_integer</th><th scope=col>root_roman</th><th scope=col>quality</th><th scope=col>title_compressed</th><th scope=col>artist_compressed</th><th scope=col>title</th><th scope=col>artist</th></tr>
	<tr><th scope=col>&lt;int&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1961</td><td>A:min</td><td>9</td><td>VI</td><td>min</td><td>idon'tmind</td><td>jamesbrown</td><td>I Don't Mind</td><td>James Brown</td></tr>
	<tr><td>1961</td><td>C:maj</td><td>0</td><td>I </td><td>maj</td><td>idon'tmind</td><td>jamesbrown</td><td>I Don't Mind</td><td>James Brown</td></tr>
	<tr><td>1961</td><td>A:min</td><td>9</td><td>VI</td><td>min</td><td>idon'tmind</td><td>jamesbrown</td><td>I Don't Mind</td><td>James Brown</td></tr>
	<tr><td>1961</td><td>C:maj</td><td>0</td><td>I </td><td>maj</td><td>idon'tmind</td><td>jamesbrown</td><td>I Don't Mind</td><td>James Brown</td></tr>
	<tr><td>1961</td><td>A:min</td><td>9</td><td>VI</td><td>min</td><td>idon'tmind</td><td>jamesbrown</td><td>I Don't Mind</td><td>James Brown</td></tr>
	<tr><td>1961</td><td>C:maj</td><td>0</td><td>I </td><td>maj</td><td>idon'tmind</td><td>jamesbrown</td><td>I Don't Mind</td><td>James Brown</td></tr>
</tbody>
</table>




```R
# These packages need to be loaded in the first `@tests` cell. 
library(testthat) 
library(IRkernel.testthat)

run_tests({
    test_that("Read in data correctly.", {
        expect_is(bb, "tbl_df", 
            info = 'You should use read_csv (with an underscore) to read "datasets/bb_chords.csv" into bb')
    })
    
    test_that("Read in data correctly.", {
        bb_correct <- read_csv('datasets/bb_chords.csv')
        expect_equivalent(bb, bb_correct, 
            info = 'bb should contain the data in "datasets/bb_chords.csv"')
    })
})
```

    
    Attaching package: 'testthat'
    
    The following object is masked from 'package:dplyr':
    
        matches
    







    0/2 tests passed
    > fail :: Read in data correctly.
    `bb` inherits from `data.frame` not `tbl_df`.
    You should use read_csv (with an underscore) to read "datasets/bb_chords.csv" into bb
     
    ---
    > fail :: Read in data correctly.
    `bb` not equivalent to `bb_correct`.
    Component "chord": 'current' is not a factor
    Component "root_integer": 'current' is not a factor
    Component "root_roman": 'current' is not a factor
    Component "quality": 'current' is not a factor
    Component "title_compressed": 'current' is not a factor
    Component "artist_compressed": 'current' is not a factor
    Component "title": 'current' is not a factor
    Component "artist": 'current' is not a factor
    bb should contain the data in "datasets/bb_chords.csv"
     
    ---


## 2. The most common chords
<p>As seen in the previous task, this is a <em>tidy</em> dataset: each row represents a single observation, and each column a particular variable or attribute of that observation. Note that the metadata for each song (title, artist, year) is repeated for each chord -- like "I Don't Mind" by James Brown, 1961 -- while the unique attributes of each chord (chord symbol, chord quality, and analytical designations like integer and Roman-numeral notation) is included once for each chord change.</p>
<p>A key element of the style of any popular musical artist is the kind of chords they use in their songs. But not all chords are created equal! In addition to differences in how they sound, some chords are simply easier to play than others. On top of that, some chords are easier to play on one instrument than they are on another. And while master musicians can play a wide variety of chords and progressions with ease, it's not a stretch to think that even the best musicians may choose more "idiomatic" chords and progressions for their instrument.</p>
<p>To start to explore that, let's look at the most common chords in the McGill Billboard Dataset.</p>


```R
# Counting the most common chords
bb_count <- bb %>% count(chord, sort = T)

# Displaying the top 20 chords
# .... YOUR CODE FOR TASK 2 ....
bb_count[1:20,]
```


<table>
<caption>A tibble: 20 x 2</caption>
<thead>
	<tr><th scope=col>chord</th><th scope=col>n</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>C:maj </td><td>1183</td></tr>
	<tr><td>G:maj </td><td>1140</td></tr>
	<tr><td>A:maj </td><td>1071</td></tr>
	<tr><td>D:maj </td><td>1054</td></tr>
	<tr><td>F:maj </td><td> 859</td></tr>
	<tr><td>E:maj </td><td> 839</td></tr>
	<tr><td>Bb:maj</td><td> 718</td></tr>
	<tr><td>B:maj </td><td> 503</td></tr>
	<tr><td>Ab:maj</td><td> 375</td></tr>
	<tr><td>Eb:maj</td><td> 360</td></tr>
	<tr><td>A:min </td><td> 328</td></tr>
	<tr><td>E:min </td><td> 298</td></tr>
	<tr><td>Db:maj</td><td> 293</td></tr>
	<tr><td>D:min </td><td> 250</td></tr>
	<tr><td>B:min </td><td> 236</td></tr>
	<tr><td>N     </td><td> 201</td></tr>
	<tr><td>E:min7</td><td> 186</td></tr>
	<tr><td>C:min </td><td> 176</td></tr>
	<tr><td>D:7   </td><td> 176</td></tr>
	<tr><td>A:min7</td><td> 170</td></tr>
</tbody>
</table>




```R
run_tests({
    test_that("bb_count is correct", {
        correct_bb_count <- bb %>%
          count(chord, sort = TRUE)
        expect_equivalent(bb_count, correct_bb_count, 
            info = "bb_count should contain the count of each type of chord.")
    })
})
```






    1/1 tests passed


## 3. Visualizing the most common chords
<p>Of course, it's easier to get a feel for just how common some of these chords are if we graph them and show the percentage of the total chord count represented by each chord.
Musicians may notice right away that the most common chords in this corpus are chords that are easy to play on both the guitar and the piano: C, G, A, and D major — and to an extent, F and E major. (They also belong to keys, or scales, that are easy to play on most instruments, so they fit well with melodies and solos, as well.) After that, there is a steep drop off in the frequency with which individual chords appear. </p>
<p>To illustrate this, here is a short video demonstrating the relative ease (and difficulty) of some of the most common (and not-so-common) chords in the McGill Billboard dataset.
<br><br>
<a href="https://player.vimeo.com/video/251381886" target="blank_"><img style="max-width: 500px;" src="https://assets.datacamp.com/production/project_78/img/smaller_video_screenshot.jpeg"></a></p>


```R
# Creating a bar plot from bb_count
bb_count %>%
  slice(1:20) %>%
  mutate(share = (n/sum(n))*100,
         chord = reorder(chord,share)) %>%
  ggplot(aes(x=chord, y=share, fill=chord)) +
  geom_col()+
  coord_flip() +
  xlab("Common Chords") +
  ylab("Frequent Occurence") 
```


![png](output_7_0.png)



```R
run_tests({
    test_that("bb_count has some data in it", {
    expect_true(length(bb_count) > 0, 
        info = "Looks like you're missing data in `bb_count`.")
    })
})
```






    1/1 tests passed


## 4. Chord "bigrams"
<p>Just as some chords are more common and more idiomatic than others, not all chord <em>progressions</em> are created equal. To look for common patterns in the structuring of chord progressions, we can use many of the same modes of analysis used in text-mining to analyze phrases. A chord change is simply a <em>bigram</em> — a two-"word" phrase — composed of a starting chord and a following chord. Here are the most common two-chord "phrases" in the McGill Billboard dataset.
To help you get your ear around some of these common progressions, here's a short audio clip containing some of the most common chord bigrams.
<br><br></p>
<audio controls src="http://assets.datacamp.com/production/project_79/img/bigrams.mp3">
  Your browser does not support the audio tag.
</audio>


```R
# Wrangling and counting bigrams
bb_bigram_count <- bb %>% mutate(next_chord = lead(chord), next_title = lead(title), bigram = paste(chord,next_chord)) %>% filter(title == next_title) %>% count(bigram,sort=T)
    # .... YOUR CODE FOR TASK 4 ....

# Displaying the first 20 rows of bb_bigram_count
# .... YOUR CODE FOR TASK 4 ....
bb_bigram_count[1:20, ]
```


<table>
<caption>A tibble: 20 x 2</caption>
<thead>
	<tr><th scope=col>bigram</th><th scope=col>n</th></tr>
	<tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>G:maj D:maj </td><td>241</td></tr>
	<tr><td>C:maj F:maj </td><td>234</td></tr>
	<tr><td>C:maj G:maj </td><td>217</td></tr>
	<tr><td>B:maj E:maj </td><td>202</td></tr>
	<tr><td>F:maj C:maj </td><td>195</td></tr>
	<tr><td>A:maj E:maj </td><td>190</td></tr>
	<tr><td>A:maj D:maj </td><td>189</td></tr>
	<tr><td>D:maj G:maj </td><td>185</td></tr>
	<tr><td>G:maj C:maj </td><td>185</td></tr>
	<tr><td>D:maj A:maj </td><td>179</td></tr>
	<tr><td>E:maj A:maj </td><td>175</td></tr>
	<tr><td>F:maj Bb:maj</td><td>143</td></tr>
	<tr><td>Bb:maj F:maj</td><td>134</td></tr>
	<tr><td>E:maj B:maj </td><td>134</td></tr>
	<tr><td>Bb:maj C:maj</td><td>133</td></tr>
	<tr><td>G:maj A:maj </td><td>133</td></tr>
	<tr><td>A:maj B:maj </td><td>112</td></tr>
	<tr><td>A:maj G:maj </td><td>105</td></tr>
	<tr><td>F:maj G:maj </td><td> 99</td></tr>
	<tr><td>D:maj C:maj </td><td> 93</td></tr>
</tbody>
</table>




```R
run_tests({
    test_that("bb_bigram_count is correct", {
      correct_bb_bigram_count <- bb %>%
      mutate(next_chord = lead(chord),
             next_title = lead(title),
             bigram = paste(chord, next_chord)) %>%
      filter(title == next_title) %>%
      count(bigram, sort = TRUE)
    expect_equivalent(bb_bigram_count, correct_bb_bigram_count, 
        info = "`bb_bigram_count` should contain the count of each type of bigram. Don't forget to sort by bigram frequency!")
    })
})
```






    1/1 tests passed


## 5. Visualizing the most common chord progressions
<p>We can get a better sense of just how popular some of these chord progressions are if we plot them on a bar graph. Note how the most common chord change, G major to D major, occurs more than twice as often than even some of the other top 20 chord bigrams.</p>


```R
# Creating a column plot from bb_bigram_count
bb_bigram_count %>%
  slice(1:20) %>%
  mutate(share = (n/sum(n)) *100,
         chord =  reorder(bigram,share)) %>%
   ggplot(aes(x=chord, y=share, fill=chord)) +
  geom_col()+
  coord_flip() +
  xlab("Common Next_Chords") +
  ylab("Frequent Occurence")
```


![png](output_13_0.png)



```R
run_tests({
    test_that("bb_bigram_count has some data in it", {
    expect_true(length(bb_bigram_count) > 0, 
        info = "Looks like you're missing data in `bb_bigram_count`.")
    })
})
```






    1/1 tests passed


## 6. Finding the most common artists
<p>As noted above, the most common chords (and chord bigrams) are those that are easy to play on both the guitar and the piano. If the degree to which these chords are idiomatic on guitar or piano (or both) <em>determine</em> how common they are, we would expect to find the more idiomatic guitar chords (C, G, D, A, and E major) to be more common in guitar-driven songs, but we would expect the more idiomatic piano chords (C, F, G, D, and B-flat major) to be more common in piano-driven songs. (Note that there is some overlap between these two instruments.)</p>
<p>The McGill Billboard dataset does not come with songs tagged as "piano-driven" or "guitar-driven," so to test this hypothesis, we'll have to do that manually. Rather than make this determination for every song in the corpus, let's focus on just a few to see if the hypothesis has some validity. If so, then we can think about tagging more artists in the corpus and testing the hypothesis more exhaustively.</p>
<p>Here are the 30 artists with the most songs in the corpus. From this list, we'll extract a few artists who are obviously heavy on guitar or piano to compare.</p>


```R
# Finding 30 artists with the most songs in the corpus
bb_30_artists <- bb %>% select(artist, title) %>% unique() %>% count(artist, sort = T)
    #.... YOUR CODE FOR TASK 6 ....

# Displaying 30 artists with the most songs in the corpus
#.... YOUR CODE FOR TASK 6 ....
bb_30_artists %>% slice(1:30)
```


<table>
<caption>A tibble: 30 x 2</caption>
<thead>
	<tr><th scope=col>artist</th><th scope=col>n</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>Elvis Presley     </td><td>13</td></tr>
	<tr><td>Brenda Lee        </td><td> 9</td></tr>
	<tr><td>Dion              </td><td> 8</td></tr>
	<tr><td>Bob Seger         </td><td> 7</td></tr>
	<tr><td>James Brown       </td><td> 7</td></tr>
	<tr><td>Kenny Rogers      </td><td> 7</td></tr>
	<tr><td>The Beatles       </td><td> 7</td></tr>
	<tr><td>Chicago           </td><td> 6</td></tr>
	<tr><td>Dr. Hook          </td><td> 6</td></tr>
	<tr><td>Eric Clapton      </td><td> 6</td></tr>
	<tr><td>John Denver       </td><td> 6</td></tr>
	<tr><td>Johnny Tillotson  </td><td> 6</td></tr>
	<tr><td>The Beach Boys    </td><td> 6</td></tr>
	<tr><td>Abba              </td><td> 5</td></tr>
	<tr><td>Billy Idol        </td><td> 5</td></tr>
	<tr><td>Cliff Richard     </td><td> 5</td></tr>
	<tr><td>Glen Campbell     </td><td> 5</td></tr>
	<tr><td>The Rolling Stones</td><td> 5</td></tr>
	<tr><td>Billy Joel        </td><td> 4</td></tr>
	<tr><td>Cheap Trick       </td><td> 4</td></tr>
	<tr><td>Cyndi Lauper      </td><td> 4</td></tr>
	<tr><td>David Bowie       </td><td> 4</td></tr>
	<tr><td>Elton John        </td><td> 4</td></tr>
	<tr><td>Genesis           </td><td> 4</td></tr>
	<tr><td>Heart             </td><td> 4</td></tr>
	<tr><td>Jackson Browne    </td><td> 4</td></tr>
	<tr><td>Little River Band </td><td> 4</td></tr>
	<tr><td>Michael Jackson   </td><td> 4</td></tr>
	<tr><td>Pat Benatar       </td><td> 4</td></tr>
	<tr><td>Stevie Wonder     </td><td> 4</td></tr>
</tbody>
</table>




```R
run_tests({
    test_that("bb artists counted and sorted", {
      correct_bb_30_artists <- bb %>%
        select(artist, title) %>%
        unique() %>%
        count(artist, sort = TRUE)
    expect_equivalent(bb_30_artists, correct_bb_30_artists, 
        info = "`bb_30_artists` should contain the number of soungs (not chords) by each artist in the corpus. Don't forget to sort!")
    })
})
```






    1/1 tests passed


## 7. Tagging the corpus
<p>There are relatively few artists in this list whose music is demonstrably "piano-driven," but we can identify a few that generally emphasize keyboards over guitar: Abba, Billy Joel, Elton John, and Stevie Wonder — totaling 17 songs in the corpus. There are many guitar-centered artists in this list, so for our test, we'll focus on three well known, guitar-heavy artists with a similar number of songs in the corpus: The Rolling Stones, The Beatles, and Eric Clapton (18 songs).</p>
<p>Once we've subset the corpus to only songs by these seven artists and applied the "piano" and "guitar" tags, we can compare the chord content of piano-driven and guitar-driven songs.</p>


```R
tags <- tibble(
  artist = c('Abba', 'Billy Joel', 'Elton John', 'Stevie Wonder', 'The Rolling Stones', 'The Beatles', 'Eric Clapton'),
  instrument = c('piano', 'piano', 'piano', 'piano', 'guitar', 'guitar', 'guitar'))

# Creating a new dataframe bb_tagged that includes a new column instrument from tags
bb_tagged <- bb %>% inner_join(tags)
    # .... YOUR CODE FOR TASK 7 ....
    
# Displaying the new data frame
# .... YOUR CODE FOR TASK 7 ....
bb_tagged
```

    Joining, by = "artist"
    Warning message:
    "Column `artist` joining factor and character vector, coercing into character vector"


<table>
<caption>A data.frame: 1101 x 10</caption>
<thead>
	<tr><th scope=col>year</th><th scope=col>chord</th><th scope=col>root_integer</th><th scope=col>root_roman</th><th scope=col>quality</th><th scope=col>title_compressed</th><th scope=col>artist_compressed</th><th scope=col>title</th><th scope=col>artist</th><th scope=col>instrument</th></tr>
	<tr><th scope=col>&lt;int&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1984</td><td>C:maj   </td><td>0 </td><td>I   </td><td>maj  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>D:min   </td><td>2 </td><td>II  </td><td>min  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>F:maj   </td><td>5 </td><td>IV  </td><td>maj  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>G:maj   </td><td>7 </td><td>V   </td><td>maj  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>C:maj   </td><td>0 </td><td>I   </td><td>maj  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>D:min   </td><td>2 </td><td>II  </td><td>min  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>F:maj   </td><td>5 </td><td>IV  </td><td>maj  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>G:maj   </td><td>7 </td><td>V   </td><td>maj  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>C:maj   </td><td>0 </td><td>I   </td><td>maj  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>G:min7  </td><td>7 </td><td>V   </td><td>min7 </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>C:maj/5 </td><td>0 </td><td>I   </td><td>maj/5</td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>Bb:maj/5</td><td>10</td><td>bVII</td><td>maj/5</td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>F:maj   </td><td>5 </td><td>IV  </td><td>maj  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>G:maj   </td><td>7 </td><td>V   </td><td>maj  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>C:maj   </td><td>0 </td><td>I   </td><td>maj  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>D:min   </td><td>2 </td><td>II  </td><td>min  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>F:maj   </td><td>5 </td><td>IV  </td><td>maj  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>G:maj   </td><td>7 </td><td>V   </td><td>maj  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>C:maj   </td><td>0 </td><td>I   </td><td>maj  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>D:min   </td><td>2 </td><td>II  </td><td>min  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>F:maj   </td><td>5 </td><td>IV  </td><td>maj  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1984</td><td>G:maj   </td><td>7 </td><td>V   </td><td>maj  </td><td>aninnocentman</td><td>billyjoel  </td><td>An Innocent Man</td><td>Billy Joel  </td><td>piano </td></tr>
	<tr><td>1972</td><td>A:maj   </td><td>7 </td><td>V   </td><td>maj  </td><td>letitrain    </td><td>ericclapton</td><td>Let It Rain    </td><td>Eric Clapton</td><td>guitar</td></tr>
	<tr><td>1972</td><td>G:maj/9 </td><td>5 </td><td>IV  </td><td>maj/9</td><td>letitrain    </td><td>ericclapton</td><td>Let It Rain    </td><td>Eric Clapton</td><td>guitar</td></tr>
	<tr><td>1972</td><td>A:maj   </td><td>7 </td><td>V   </td><td>maj  </td><td>letitrain    </td><td>ericclapton</td><td>Let It Rain    </td><td>Eric Clapton</td><td>guitar</td></tr>
	<tr><td>1972</td><td>G:maj/9 </td><td>5 </td><td>IV  </td><td>maj/9</td><td>letitrain    </td><td>ericclapton</td><td>Let It Rain    </td><td>Eric Clapton</td><td>guitar</td></tr>
	<tr><td>1972</td><td>A:maj   </td><td>7 </td><td>V   </td><td>maj  </td><td>letitrain    </td><td>ericclapton</td><td>Let It Rain    </td><td>Eric Clapton</td><td>guitar</td></tr>
	<tr><td>1972</td><td>D:maj   </td><td>0 </td><td>I   </td><td>maj  </td><td>letitrain    </td><td>ericclapton</td><td>Let It Rain    </td><td>Eric Clapton</td><td>guitar</td></tr>
	<tr><td>1972</td><td>A:min   </td><td>7 </td><td>V   </td><td>min  </td><td>letitrain    </td><td>ericclapton</td><td>Let It Rain    </td><td>Eric Clapton</td><td>guitar</td></tr>
	<tr><td>1972</td><td>C:maj   </td><td>10</td><td>bVII</td><td>maj  </td><td>letitrain    </td><td>ericclapton</td><td>Let It Rain    </td><td>Eric Clapton</td><td>guitar</td></tr>
	<tr><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td></tr>
	<tr><td>1965</td><td>F#:min  </td><td>9          </td><td>VI         </td><td>min        </td><td>help!         </td><td>thebeatles      </td><td>Help!           </td><td>The Beatles       </td><td>guitar</td></tr>
	<tr><td>1965</td><td>D:maj   </td><td>5          </td><td>IV         </td><td>maj        </td><td>help!         </td><td>thebeatles      </td><td>Help!           </td><td>The Beatles       </td><td>guitar</td></tr>
	<tr><td>1965</td><td>G:maj   </td><td>10         </td><td>bVII       </td><td>maj        </td><td>help!         </td><td>thebeatles      </td><td>Help!           </td><td>The Beatles       </td><td>guitar</td></tr>
	<tr><td>1965</td><td>A:maj   </td><td>0          </td><td>I          </td><td>maj        </td><td>help!         </td><td>thebeatles      </td><td>Help!           </td><td>The Beatles       </td><td>guitar</td></tr>
	<tr><td>1965</td><td>C#:min  </td><td>4          </td><td>III        </td><td>min        </td><td>help!         </td><td>thebeatles      </td><td>Help!           </td><td>The Beatles       </td><td>guitar</td></tr>
	<tr><td>1965</td><td>F#:maj  </td><td>9          </td><td>VI         </td><td>maj        </td><td>help!         </td><td>thebeatles      </td><td>Help!           </td><td>The Beatles       </td><td>guitar</td></tr>
	<tr><td>1965</td><td>D:maj   </td><td>5          </td><td>IV         </td><td>maj        </td><td>help!         </td><td>thebeatles      </td><td>Help!           </td><td>The Beatles       </td><td>guitar</td></tr>
	<tr><td>1965</td><td>G:maj   </td><td>10         </td><td>bVII       </td><td>maj        </td><td>help!         </td><td>thebeatles      </td><td>Help!           </td><td>The Beatles       </td><td>guitar</td></tr>
	<tr><td>1965</td><td>A:maj   </td><td>0          </td><td>I          </td><td>maj        </td><td>help!         </td><td>thebeatles      </td><td>Help!           </td><td>The Beatles       </td><td>guitar</td></tr>
	<tr><td>1965</td><td>B:min   </td><td>2          </td><td>II         </td><td>min        </td><td>help!         </td><td>thebeatles      </td><td>Help!           </td><td>The Beatles       </td><td>guitar</td></tr>
	<tr><td>1965</td><td>B:min/b7</td><td>2          </td><td>II         </td><td>min/b7     </td><td>help!         </td><td>thebeatles      </td><td>Help!           </td><td>The Beatles       </td><td>guitar</td></tr>
	<tr><td>1969</td><td>N       </td><td>NonHarmonic</td><td>NonHarmonic</td><td>NonHarmonic</td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
	<tr><td>1969</td><td>G:maj   </td><td>0          </td><td>I          </td><td>maj        </td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
	<tr><td>1969</td><td>C:maj   </td><td>5          </td><td>IV         </td><td>maj        </td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
	<tr><td>1969</td><td>C:sus4  </td><td>5          </td><td>IV         </td><td>sus4       </td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
	<tr><td>1969</td><td>C:maj   </td><td>5          </td><td>IV         </td><td>maj        </td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
	<tr><td>1969</td><td>G:maj   </td><td>0          </td><td>I          </td><td>maj        </td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
	<tr><td>1969</td><td>A:maj   </td><td>2          </td><td>II         </td><td>maj        </td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
	<tr><td>1969</td><td>D:maj   </td><td>7          </td><td>V          </td><td>maj        </td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
	<tr><td>1969</td><td>D:sus4  </td><td>7          </td><td>V          </td><td>sus4       </td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
	<tr><td>1969</td><td>D:maj   </td><td>7          </td><td>V          </td><td>maj        </td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
	<tr><td>1969</td><td>G:maj   </td><td>0          </td><td>I          </td><td>maj        </td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
	<tr><td>1969</td><td>C:maj   </td><td>5          </td><td>IV         </td><td>maj        </td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
	<tr><td>1969</td><td>C:sus4  </td><td>5          </td><td>IV         </td><td>sus4       </td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
	<tr><td>1969</td><td>C:maj   </td><td>5          </td><td>IV         </td><td>maj        </td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
	<tr><td>1969</td><td>G:maj   </td><td>0          </td><td>I          </td><td>maj        </td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
	<tr><td>1969</td><td>D:maj   </td><td>7          </td><td>V          </td><td>maj        </td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
	<tr><td>1969</td><td>G:maj   </td><td>0          </td><td>I          </td><td>maj        </td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
	<tr><td>1969</td><td>D:maj   </td><td>7          </td><td>V          </td><td>maj        </td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
	<tr><td>1969</td><td>G:maj   </td><td>0          </td><td>I          </td><td>maj        </td><td>honkytonkwomen</td><td>therollingstones</td><td>Honky Tonk Women</td><td>The Rolling Stones</td><td>guitar</td></tr>
</tbody>
</table>




```R
run_tests({
    test_that("bb artists counted and sorted", {
      correct_bb_tagged <- bb %>%
        inner_join(tags)
    expect_equivalent(bb_tagged, correct_bb_tagged, 
        info = "`bb_tagged` should be a successful join of `bb` and `tags` that only contains records cointained in both dataframes.")
    })
})
```






    1/1 tests passed


## 8. Comparing chords in piano-driven and guitar-driven songs
<p>Let's take a look at any difference in how common chords are in these two song groups. To clean things up, we'll just focus on the 20 chords most common in the McGill Billboard dataset overall.</p>
<p>While we want to be careful about drawing any conclusions from such a small set of songs, we can see that the chords easiest to play on the guitar <em>do</em> dominate the guitar-driven songs, especially G, D, E, and C major, as well as A major and minor. Similarly, "flat" chords (B-flat, E-flat, A-flat major) occur frequently in piano-driven songs, though they are nearly absent from the guitar-driven songs. In fact, the first and fourth most frequent piano chords are "flat" chords that occur rarely, if at all, in the guitar songs.</p>
<p>So with all the appropriate caveats, it seems like the instrument-based-harmony hypothesis does have some merit and is worth further examination.</p>


```R
# The top 20 most common chords
top_20 <- bb_count$chord[1:20]

# Comparing the frequency of the 20 most common chords in piano- and guitar-driven songs
bb_tagged %>%
  filter(chord %in% top_20) %>%
  count(chord, instrument, sort = T) %>%
  ggplot(aes(chord,n, fill = chord)) +
  geom_bar(stat = "identity") +
  facet_grid(~instrument) +
  coord_flip() +
  xlab("Common Chords") +
  ylab("No. of Occurence") 

```


![png](output_22_0.png)



```R
run_tests({
    test_that("bb_tagged has some data in it", {
    expect_true(length(bb_tagged) > 0, 
        info = "Looks like you're missing data in `bb_tagged`.")
    })
})
```






    1/1 tests passed


## 9. Comparing chord bigrams in piano-driven and guitar-driven songs
<p>Since chord occurrence and chord bigram occurrence are naturally strongly tied to each other, it would not be a reach to expect that a difference in chord frequency would be reflected in a difference in chord bigram frequency. Indeed that is what we find.</p>


```R
# The top 20 most common bigrams
top_20_bigram <- bb_bigram_count$bigram[1:20]

# Creating a faceted plot comparing guitar- and piano-driven songs for bigram frequency
bb_tagged %>%
  mutate(next_chord = lead(chord),
         next_title = lead(title),
         bigram = paste(chord, next_chord)) %>%
  filter(title == next_title) %>%
  count(bigram, instrument, sort = TRUE) %>%
  filter(bigram %in% top_20_bigram) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_col() +
  facet_grid(~instrument) +
  coord_flip() +
  ylab('Total bigrams') +
  xlab('Bigram') +
  theme(legend.position="none")

  # .... MODIFIED CODE FROM TASK 4 .... 
  # .... MODIFIED CODE FROM TASK 8 ....
```


![png](output_25_0.png)



```R
run_tests({
    test_that("bb_bigram_count has some data in it", {
    expect_true(length(bb_bigram_count) > 0, 
        info = "Looks like you're missing data in `bb_bigram_count`.")
    })
})
```






    1/1 tests passed


## 10. Conclusion
<p>We set out asking if the degree to which a chord is "idiomatic" on an instrument affects how frequently it is used by a songwriter. It seems that is indeed the case. In a large representative sample of pop/rock songs from the historical Billboard charts, the chords most often learned first by guitarists and pianists are the most common. In fact, chords commonly deemed <em>easy</em> or <em>beginner-friendly</em> on <strong>both</strong> piano and guitar are far and away the most common in the corpus.</p>
<p>We also examined a subset of 35 songs from seven piano- and guitar-heavy artists and found that guitarists and pianists tend to use different sets of chords for their songs. This was an extremely small (and likely not representative) sample, so we can do nothing more than hypothesize that this trend might carry over throughout the larger dataset. But it seems from this exploration that it's worth a closer look.</p>
<p>There are still more questions to explore with this dataset. What about band-driven genres like classic R&amp;B and funk, where artists like James Brown and Chicago build chords from a large number of instruments each playing a single note? What about "progressive" bands like Yes and Genesis, where "easy" and "idiomatic" may be less of a concern during the songwriting process? And what if we compared this dataset to a collection of chords from classical songs, jazz charts, folk songs, liturgical songs?</p>
<p>There's only one way to find out!</p>


```R
# Set to TRUE or FALSE to reflect your answer
hypothesis_valid <- TRUE

# Set to TRUE or FALSE to reflect your answer
more_data_needed <- TRUE
```


```R
run_tests({
    test_that("hypothesis is true", {
    expect_true(hypothesis_valid, 
        info = "Are you sure the hypothesis isn't valid?!")
    })
    test_that("more_data_needed is true", {
    expect_true(more_data_needed, 
        info = "Are you sure we don't need more data?!")
    })
})
```






    2/2 tests passed

