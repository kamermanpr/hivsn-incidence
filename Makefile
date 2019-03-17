# Create directories if required
$(shell mkdir -p data-cleaned figures outputs outputs/figures/)

# Dummy outputs
DATA = 	data-cleaned/clean_data.csv \
		data-cleaned/clean_data.rds

S1 = 	outputs/suppl-01-descriptive-whole-cohort.pdf

S2 = 	outputs/suppl-02-descriptive-by-SN-status.pdf

S3 = 	outputs/suppl-03-follow-up-analysis.pdf

S4 = 	outputs/suppl-04-incidence-analysis.pdf

.PHONY: all

all: 	$(DATA) $(S1) $(S2) $(S3) $(S4)

# Clean
clean:
	rm -rfv figures/ outputs/ data-cleaned/

# Generate data
data-cleaned/clean_data.csv data-cleaned/clean_data.rds: \
clean-data.R \
data-original/deidentified-data-august-2017.120.xlsx
	Rscript "$<"

# Generate outputs
outputs/suppl-01-descriptive-whole-cohort.pdf: \
suppl-01-descriptive-whole-cohort.Rmd \
data-cleaned/clean_data.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl-01-descriptive-whole-cohort outputs/figures/

outputs/suppl-02-descriptive-by-SN-status.pdf: \
suppl-02-descriptive-by-SN-status.Rmd \
data-cleaned/clean_data.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl-02-descriptive-by-SN-status outputs/figures/

outputs/suppl-03-follow-up-analysis.pdf: \
suppl-03-follow-up-analysis.Rmd \
data-cleaned/clean_data.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl-03-follow-up-analysis outputs/figures/

outputs/suppl-04-incidence-analysis.pdf: \
suppl-04-incidence-analysis.Rmd \
data-cleaned/clean_data.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl-04-incidence-analysis outputs/figures/
