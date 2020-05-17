# covid19

This is a system for graphing covid19 incidence over time.

To add a new country, do the following (and, please, make a pull request).
Note that the steps do not require R skill.  The only skills you need are the
ability to use a plain-text editor, an understanding of how to make a pull
request on github, and a unix machine (e.g. linux, or a mac set up with `make`,
via Developer Tools).  While editing, *do not* add line breaks that do not
match local patterns, and *do not* merge together adjacent lines; also, please
resist the temptation to change any text other than as instructed below.

1. Find the "proper" name for the country in the source file https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv.  This proper name must be used, not whatever you regard the name to be.
2. Add the name to the `Makefile`, along with the other names. Put it in
   alphabetical order.  Be very careful not to permit your text editor to add a line break.
3. Look in the `population.R` file, which uses another .csv file to get country
populations.  If this file names your country differently than in step 1, you must
add a line to `population.R` to rename the nation.  Look near line 21.
3. Edit the `index.html` file.  Look for the line containing the text `Shortcut
   links:`. Add the new country in the list that follows that line, placing it
in alphabetic order with respect to the others.
4. Still within `index.html`, search down for the country name that occurs just below the one
you added.  You will get to a block of two lines, with a blank line above. Copy those three
lines and paste the copy above.  Now, you will see a duplicated entry.  Move your
cursor to the start of the first of the pair, and change the country name in
the three spots where it occurs.
5. If you know how, do a `git diff` to see the changes.  Assure yourself that you
have followed the proceeding steps correctly.
6. Type `make` to rebuild the site.  If this produces errors, it will be up to you
to fix them, for they almost certainly mean that if I incorporate your changes,
it will break the site for everyone.
7. Once you are assured that things are working, generate a git "pull request" on github.com.

Note that if your pull request breaks the site, you will have wasted your time,
and mine.

