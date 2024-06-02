from flask import Flask, render_template, request, redirect, url_for
import sqlite3
import base64
import matplotlib.pyplot as plt
import io
import csv
from datetime import datetime
import seaborn as sns

# Set Matplotlib to use the 'Agg' backend
plt.switch_backend('Agg')

app = Flask(__name__)

DATABASE = 'zip_c.db'

def get_db():
    conn = sqlite3.connect(DATABASE)
    return conn

# Load zip code coordinates from CSV
zip_code_coordinates = {}
with open('uszips.csv', newline='') as csvfile:
    reader = csv.DictReader(csvfile)
    for row in reader:
        zip_code_coordinates[row['zip']] = {'lat': float(row['lat']), 'lng': float(row['lng'])}

@app.route('/')
def home():
    return render_template('home.html')

@app.route('/about')
def about():
    return render_template('about.html')

@app.route('/source')
def source():
    return render_template('source.html')

@app.route('/by_room_type_and_region')
def by_room_type_and_region():
    return redirect("https://wangzx32.shinyapps.io/zillowhousingshiny/")

@app.route('/by_zip_code', methods=['GET', 'POST'])
def by_zip_code():
    if request.method == 'POST':
        zip_codes = request.form['zip_codes']
        start_date = request.form['start_date']
        end_date = request.form['end_date']
        return redirect(url_for('select_type', zip_codes=zip_codes, start_date=start_date, end_date=end_date))
    return render_template('index.html')

@app.route('/select_type/<zip_codes>/<start_date>/<end_date>', methods=['GET', 'POST'])
def select_type(zip_codes, start_date, end_date):
    zip_codes = zip_codes.split(',')
    coordinates_list = [zip_code_coordinates.get(zip_code.strip(), {"lat": 0, "lng": 0}) for zip_code in zip_codes]
    if request.method == 'POST':
        house_type = request.form.get('house_type')
        return redirect(url_for('show_prices', zip_codes=','.join(zip_codes), house_type=house_type, start_date=start_date, end_date=end_date))
    return render_template('select_type.html', zip_codes=zip_codes, start_date=start_date, end_date=end_date, coordinates_list=coordinates_list)

@app.route('/show_prices/<zip_codes>/<house_type>/<start_date>/<end_date>')
def show_prices(zip_codes, house_type, start_date, end_date):
    conn = get_db()
    cur = conn.cursor()
    zip_codes = zip_codes.split(',')
    table_name = f'zip_{house_type}'

    # Convert dates to year and month
    start_year, start_month = start_date.split('-')
    end_year, end_month = end_date.split('-')

    data = {}
    for zip_code in zip_codes:
        query = f"""
        SELECT Year, Month, Avg_Pricing_Month FROM {table_name}
        WHERE Zipcode = ? AND ((Year = ? AND Month >= ?) OR (Year = ? AND Month <= ?) OR (Year > ? AND Year < ?))
        ORDER BY Year, Month
        """
        cur.execute(query, (zip_code.strip(), start_year, start_month, end_year, end_month, start_year, end_year))
        data[zip_code] = cur.fetchall()

    print("Fetched Data:", data)  # Debug print

    if not any(data.values()):
        return f"No data available for the selected criteria: {house_type} in {zip_codes} from {start_date} to {end_date}"

    # Create figure and subplots with adjusted size
    sns.set(style="whitegrid")
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 12))

    # Line plot
    all_dates = []
    colors = sns.color_palette("husl", len(zip_codes))  # Generate a list of colors
    for i, zip_code in enumerate(zip_codes):
        if data[zip_code]:
            dates = [datetime.strptime(f"{row[0]}-{row[1]}", "%Y-%m") for row in data[zip_code]]
            prices = [row[2] for row in data[zip_code]]
            all_dates.extend(dates)
            ax1.plot(dates, prices, marker='o', linestyle='-', color=colors[i], label=f'Zip Code {zip_code}')
    
    # Set x-axis limits to include the full range of dates
    if all_dates:
        ax1.set_xlim(min(all_dates), max(all_dates))

    ax1.set_title(f'House Prices for {house_type.replace("_", " ").title()}')
    ax1.set_xlabel('Date')
    ax1.set_ylabel('Price')
    ax1.legend()
    ax1.grid(True)

    # Price distribution histogram
    all_prices = [row[2] for zip_code in zip_codes for row in data[zip_code] if row]
    sns.histplot(all_prices, bins=20, kde=False, color="#3CB043", ax=ax2)
    ax2.set_title('Price Distribution')
    ax2.set_xlabel('Price')
    ax2.set_ylabel('Frequency')

    # Save the plot to a bytes object
    img = io.BytesIO()
    fig.tight_layout()
    plt.savefig(img, format='png')
    img.seek(0)
    plot_url = base64.b64encode(img.getvalue()).decode()

    print("Generated Plot URL:", plot_url)  # Debug print

    return render_template('show_prices.html', house_type=house_type, zip_codes=zip_codes, plot_url=plot_url)

if __name__ == '__main__':
    app.run(debug=True)