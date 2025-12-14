source("simple.R")

# Run analysis
run_pretest_analysis(
  data_path = '2026届预试-初中学生-二-身心症状.xlsx',
  # key_path = 'path/to/key.csv'"', # Optional
  output_dir = './report/',
  report_name = "初中学生身心症状"
)

