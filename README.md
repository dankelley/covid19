# cofid19

This is a system for graphing covid19 incidence over time.

To add a new country, do the following (and, please, make a pull request).
Note that the steps do not require R skill.  The only skills you need are the
ability to use a plain-text editor, and an understanding of how to make a pull
request on github.  While editing, *do not* add line breaks that do not match
local patterns, and *do not* merge together adjacent lines; also, please resist
the temptation to change any text other than as instructed below.

1. Find the "proper" name for the country in the source file https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv.  This proper name must be used, not whatever you regard the name to be.
2. Add the name to the `Makefile`, along with the other names. Put it in
   alphabetical order.  Be very careful not to permit your text editor to add a line break.
3. Edit the `index.html` file.  Look for the line containing the text `Shortcut
   links:`. Add the new country in the list that follows that line, placing it
in alphabetic order with respect to the others. Mimic the other lines, and be
sure that you place your entry in alphabetical order.
4. Still within `index.html`, search down for the country name that occurs just below the one
you added.  You will get to a block of two lines, with a blank line above. Copy those three
lines and paste the copy above.  Now, you will see a duplicated entry.  Move your
cursor to the start of the first of the pair, and change the country name in
the three spots where it occurs.
5. If you know how, do a `git diff` to see the changes.  You should see one
   addition to the `Makefile`, and four line additions to the `index.html`
file.  If you see more changes, you've done something wrong, and you should
start over because your work will get rejected when you get to step 6.
6. Generate a git "pull request".

Note that if your changes exceed those named above, your pull request will
likely be ignored, and you will have wasted your time.

These rules for adding content are designed to make it easy for you to
contribute to the project.

If you want to see whether your contribution will work, and if you are on a
unix machine, type `make` in this directory, and then load the `index.html`
file in a browser.  You should see your added country listed in the shortcut,
and with graphs.  (You may need to install some R packages for this to work.)
By doing this, you'll see what I must do, to check your pull request.
