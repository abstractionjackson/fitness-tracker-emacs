# plot.py

import sqlite3
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
import os
from pathlib import Path

# Get the directory where plot.py is located
PACKAGE_DIR = Path(__file__).parent.absolute()
DB_PATH = os.path.join(PACKAGE_DIR, "fitness.sqlite3")
PLOT_FPATH = os.path.join(PACKAGE_DIR, "fitness_plot.png")

def get_exercises():
    conn = sqlite3.connect(DB_PATH)
    cursor = conn.cursor()
    cursor.execute('SELECT * FROM exercises')
    exercises = cursor.fetchall()
    conn.close()
    return exercises

def plot_exercises():
    exercises = get_exercises()
    if not exercises:
        print("No exercise data found. Try logging some exercises first!")
        return

    df = pd.DataFrame(data=exercises)
    df.columns = ['id', 'date', 'movement', 'weight', 'sets', 'reps']
    df.sort_values(by='date', inplace=True)
    df['date'] = pd.to_datetime(df['date']).dt.strftime('%b %d')
    df['Total Reps'] = df['sets'] * df['reps']
    df['Movement'] = df['movement']

    # Create the plot
    plt.figure(figsize=(10, 6))
    sns.scatterplot(data=df, x='date', y='weight', size='Total Reps', hue='Movement')
    plt.xticks(rotation=45)
    plt.tight_layout()

    # Save the plot
    plt.savefig(PLOT_FPATH)
    plt.close()
    return True

if __name__ == '__main__':
    plot_exercises()
