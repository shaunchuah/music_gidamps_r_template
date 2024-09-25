# MUSIC and GI-DAMPs Base Analysis Template

## Requirements

1. git (to check if git is already installed, open a terminal window and type `git` and hit enter; if not installed please refer here: <https://git-scm.com/book/en/v2/Getting-Started-Installing-Git>)
2. R
3. R Studio

Tested with R 4.2.0 on both Mac and Windows

## Getting Started

### 1. Clone the git repository to your computer

Open a terminal window (in mac hit command+space and type terminal) and run the following command in the directory you wish this folder to be downloaded into (e.g. when you open the terminal initially it is in your home folder. Type `cd Downloads` to change into the Downloads folder before running the command below. )

`git clone https://github.com/shaunchuah/music_gidamps_r_template/`

### 2. Open R studio and set working directory to the folder that you have created

### 3. Install dependencies

In R Studio, run the script `installation.R`

### 3. Set up MUSIC and GI-DAMPs security tokens

Copy `example_credentials.json` into a new file and rename it `credentials.json`

Fill `credentials.json` with the API keys found on RedCap > API

Note: Depending on what data is required, your redcap account will need the correct data access group assignment.

### 4. Start running the R scripts in R studio and look at the dataframes that are generated

### 5. Run version control when you create new scripts

See tutorial here: <https://www.atlassian.com/git>

## Additional suggestions

- Create a github account
- Use this repository to kickstart your own analysis, see here: <https://docs.github.com/en/repositories/creating-and-managing-repositories/creating-a-repository-from-a-template>
