# Medical Cost Prediction Dashboard

## 1. Deskripsi Proyek

Medical Cost Prediction Dashboard merupakan aplikasi berbasis web yang dikembangkan menggunakan R dan Shiny untuk melakukan analisis serta prediksi biaya medis berdasarkan karakteristik individu.

Proyek ini menggunakan metode Regresi Linear Berganda (*Multiple Linear Regression*) untuk menganalisis hubungan antara beberapa variabel independen dengan biaya medis (*charges*) sebagai variabel dependen.

Aplikasi ini dirancang untuk mempermudah proses eksplorasi data, visualisasi, analisis regresi, pengujian asumsi klasik, serta prediksi biaya medis secara interaktif.

## 2. Tujuan Proyek

Tujuan pengembangan aplikasi ini adalah:

- Melakukan eksplorasi dan analisis terhadap data biaya medis.
- Menampilkan visualisasi data berdasarkan variabel yang tersedia.
- Membangun model Regresi Linear Berganda.
- Melakukan pengujian asumsi klasik pada model regresi.
- Melakukan pengujian signifikansi secara simultan dan parsial.
- Mengukur kemampuan model melalui koefisien determinasi.
- Menghasilkan prediksi biaya medis berdasarkan karakteristik individu.

## 3. Dataset

Dataset yang digunakan adalah **Medical Cost Personal Dataset** yang diperoleh dari Kaggle dan diunggah oleh Miri Choi.

Dataset berisi informasi mengenai karakteristik individu dan biaya medis yang dikeluarkan.

Setelah dilakukan pemeriksaan dan pembersihan data, dataset yang digunakan dalam analisis terdiri dari 1.337 observasi dan 7 variabel.

### Variabel Penelitian

| Variabel | Keterangan |
|---|---|
| age | Usia individu |
| sex | Jenis kelamin |
| bmi | Indeks massa tubuh |
| children | Jumlah anak |
| smoker | Status merokok |
| region | Wilayah tempat tinggal |
| charges | Biaya medis |

## 4. Metode Analisis

Metode utama yang digunakan dalam proyek ini adalah Regresi Linear Berganda.

Variabel dependen:
- charges

Variabel independen:
- age
- sex
- bmi
- children
- smoker
- region

Model regresi digunakan untuk mengetahui hubungan antara karakteristik individu dengan besarnya biaya medis serta menghasilkan estimasi biaya berdasarkan variabel yang dimasukkan.

## 5. Fitur Dashboard

Aplikasi memiliki beberapa menu utama sebagai berikut:

### A. Research Profile
Menampilkan informasi mengenai profil dan gambaran umum proyek penelitian.

### B. Overview
Menampilkan ringkasan hasil analisis, informasi model, serta perbandingan nilai aktual dan prediksi.

### C. Data Explorer
Menyediakan fitur eksplorasi data, penyaringan data, statistik deskriptif, dan ringkasan karakteristik dataset.

### D. Visualization
Menyediakan visualisasi data berdasarkan analisis univariat, bivariat, dan multivariat dengan berbagai pilihan grafik.

### E. Regression Analysis
Menyajikan hasil analisis regresi yang meliputi:
- Pengujian asumsi klasik.
- Persamaan regresi.
- Uji signifikansi simultan (Uji F).
- Uji signifikansi parsial (Uji t).
- Koefisien determinasi (R-Squared).
- Interpretasi hasil regresi.

### F. Kelemahan Model
Menjelaskan keterbatasan model regresi, hasil pengujian asumsi klasik yang tidak terpenuhi, serta pertimbangan dalam menginterpretasikan hasil analisis.

### G. Prediction
Menyediakan fitur prediksi biaya medis individu berdasarkan usia, jenis kelamin, BMI, jumlah anak, status merokok, dan wilayah tempat tinggal.

## 6. Teknologi dan Package

Proyek ini dikembangkan menggunakan:

- R
- RStudio
- Shiny
- shinydashboard
- DT
- Plotly
- ggplot2
- dplyr
- car
- lmtest
- sandwich

## 7. Struktur Repository

```text
Medical-Cost-Prediction/
│
├── README.md
├── UTS.R
└── insurance_clean.csv
```

Keterangan:
- `README.md`: Dokumentasi proyek.
- `UTS.R`: Sintak aplikasi dan analisis menggunakan R Shiny.
- `insurance_clean.csv`: Dataset yang telah melalui proses pembersihan data.

## 8. Cara Menjalankan Aplikasi

Untuk menjalankan aplikasi, ikuti langkah berikut:

1. Unduh atau *clone* repository ini ke komputer.
2. Buka proyek menggunakan RStudio.
3. Pastikan file `UTS.R` dan `insurance_clean.csv` berada dalam folder yang sama.
4. Pastikan seluruh package yang diperlukan telah terinstal.
5. Buka file `UTS.R` melalui RStudio.
6. Jalankan aplikasi menggunakan tombol **Run App** atau jalankan sintak melalui RStudio.

Instalasi package dapat dilakukan menggunakan perintah berikut:

```r
packages <- c(
  "shiny",
  "shinydashboard",
  "DT",
  "plotly",
  "ggplot2",
  "dplyr",
  "car",
  "lmtest",
  "sandwich"
)

install.packages(packages)
```

## 9. Catatan

Hasil prediksi yang ditampilkan oleh aplikasi merupakan estimasi berdasarkan model Regresi Linear Berganda yang dibangun dari dataset. Hasil tersebut bukan merupakan nilai tagihan medis aktual.

Hasil analisis dan prediksi juga perlu diinterpretasikan dengan mempertimbangkan karakteristik data serta keterbatasan model yang digunakan.

---

**Medical Cost Prediction Dashboard**

Developed as part of a statistical analysis project using R Shiny.
