# Goodreads Archive Stats

Computes statistics over your exported goodreads book library.

## Why

Goodreads is a very useful platform to track what books you read, which ones you plan to read, how much you read and so on. Its usefulness for tracking might arise from the fact that it is owned by Amazon, a company notorious for tracking and monitoring its users.

If you don't want to be tracked by Amazon via goodreads or just don't use Amazon products ([a justified position](https://stallman.org/amazon.html)) but still want to track your reading habits, this programme is for you. It works with exported goodreads libraries.

### Features

Currently supported features:

- Reports on what you read each year, including the total number of books, the number of read pages, your highest-rated books and most-read authors.

## Installation

### Downloading the binary

pending

### From source

1. [Install stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
2. `git clone https://github.com/gutjuri/goodreads-archive-stats.git && cd goodreads-archive-stats`
3. `stack install` or `stack install --local-bin-path .`

## Exporting your goodreads library

1. Go to goodreads.com
2. Log into your account
3. Click on "My Books"
4. Click on "Import and export"
5. Click on "Export library"
6. When the export is available, download it
7. (Optional) Go to your account settings and delete your account

## Running the programme

In the command line:

`goodreads-archive-stats < path/to/exported/library.csv`

## Maintaining your library without goodreads

You can track your reading habits even without goodreads. The exported csv file can be edited in common spreadsheet calculation programmes such as [LibreOffice Calc](https://www.libreoffice.org/discover/calc/), Microsoft Excel and Google docs. Adding new books this way takes a little more effort than using the goodreads interface, but that's the price you gotta pay for freedom. It probably doesn't even take much longer, considering goodread's slowness.
