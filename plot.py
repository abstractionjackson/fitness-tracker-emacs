from os import name
import sqlite3
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
import os

DB_PATH = os.path.expanduser("~/fitness-tracker/fitness.sqlite3")

def get_exercises():
    conn = sqlite3.connect(DB_PATH)
    cursor = conn.cursor()
    cursor.execute('SELECT * FROM exercises')
    exercises = cursor.fetchall()
    conn.close()
    return exercises

def plot_exercises():
    '''
    X-Axis: Dates at day intervals from earliest exercise date to today, format Jan 1
    Y-Axis: Weight in lbs
    Chart Type: Scatterplot
    - Point size: dynamic from total reps (sets x reps)
    - point color: movement name - distinct color by movement
    '''
    exercises = get_exercises()
    df = pd.DataFrame(exercises, columns=['id', 'date', 'movement', 'weight', 'sets', 'reps'])
    # use the date column as the index, and format the date as
    df.sort_values(by='date', inplace=True)
    df['date'] = pd.to_datetime(df['date']).dt.strftime('%b %d')
    df['Total Reps'] = df['sets'] * df['reps']
    df['Movement'] = df['movement']
    sns.scatterplot(data=df, x='date', y='weight', size='Total Reps', hue='Movement')
    plt.show()

if __name__ == '__main__':
    plot_exercises()
