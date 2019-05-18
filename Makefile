# Create directories if required
$(shell mkdir -p data-cleaned figures outputs outputs/figures)

# Dummy outputs
DATA = 	data-cleaned/clean_data.csv \
		data-cleaned/clean_data.rds

S1 = 	outputs/suppl-01-descriptive-whole-cohort.md \
		outputs/suppl-01-descriptive-whole-cohort.html

S2 = 	outputs/suppl-02-descriptive-by-SN-status.md \
		outputs/suppl-02-descriptive-by-SN-status.html

S3 = 	outputs/suppl-03-follow-up-analysis.md \
		outputs/suppl-03-follow-up-analysis.html

S4 = 	outputs/suppl-04-incidence-analysis.md \
		outputs/suppl-04-incidence-analysis.html
		
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
outputs/suppl-01-descriptive-whole-cohort.html outputs/suppl-01-descriptive-whole-cohort.md: \
suppl-01-descriptive-whole-cohort.Rmd \
data-cleaned/clean_data.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl-01-descriptive-whole-cohort outputs/figures/

outputs/suppl-02-descriptive-by-SN-status.html outputs/suppl-02-descriptive-by-SN-status.md: \
suppl-02-descriptive-by-SN-status.Rmd \
data-cleaned/clean_data.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl-02-descriptive-by-SN-status outputs/figures/

outputs/suppl-03-follow-up-analysis.html outputs/suppl-03-follow-up-analysis.md: \
suppl-03-follow-up-analysis.Rmd \
data-cleaned/clean_data.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl-03-follow-up-analysis outputs/figures/

outputs/suppl-04-incidence-analysis.html outputs/suppl-04-incidence-analysis.md: \
suppl-04-incidence-analysis.Rmd \
data-cleaned/clean_data.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl-04-incidence-analysis outputs/figures/
	