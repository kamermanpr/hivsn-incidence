# Create directories if required
$(shell mkdir -p data-cleaned figures outputs outputs/figures/)

# Move externally generated figures to appropriate folder
$(shell cp data/*.pdf figures/)

# Dummy outputs
DATA_A = 	data-cleaned/SPARS_A.csv \
			data-cleaned/SPARS_A.rds

DATA_B = 	data-cleaned/SPARS_B.csv \
			data-cleaned/SPARS_B.rds

1A = 	outputs/1A-participant-descriptive-data.md \
		outputs/1A-participant-descriptive-data.html

2Aa = 	outputs/suppl_02_2A_central-tendency-summary.md \
		outputs/suppl_02_2A_central-tendency-summary.html

2Ab = 	outputs/suppl_03_2A-central-tendency.md \
		outputs/suppl_03_2A-central-tendency.html

3A = 	outputs/suppl_04_3A-order-effects.md \
		outputs/suppl_04_3A-order-effects.html

4A1 = 	outputs/suppl_05_4A-stimulus-response-1.md \
		outputs/suppl_05_4A-stimulus-response-1.html

4A2 = 	outputs/suppl_06_4A-stimulus-response-2.md \
		outputs/suppl_06_4A-stimulus-response-2.html

4A3 = 	outputs/suppl_07_4A-stimulus-response-3.md \
		outputs/suppl_07_4A-stimulus-response-3.html

4A4 = 	outputs/suppl_08_4A-stimulus-response-4.md \
		outputs/suppl_08_4A-stimulus-response-4.html

4A5 = 	outputs/suppl_09_4A-stimulus-response-5.md \
		outputs/suppl_09_4A-stimulus-response-5.html

4A6 = 	outputs/suppl_10_4A-stimulus-response-6.md \
		outputs/suppl_10_4A-stimulus-response-6.html

4A7 = 	outputs/suppl_14_4A-figure-4-new.md \
		outputs/suppl_14_4A-figure-4-new.html

1B1 = 	outputs/suppl_11_1B-stimulus-response-1.md \
		outputs/suppl_11_1B-stimulus-response-1.html

1B2 = 	outputs/suppl_12_1B-stimulus-response-2.md \
		outputs/suppl_12_1B-stimulus-response-2.html

2B = 	outputs/suppl_13_2B-scale-agreement.md \
		outputs/suppl_13_2B-scale-agreement.html

3B = 	outputs/suppl_01_3B-instructions-for-participants.html

.PHONY: all

all: 	$(DATA_A) $(DATA_B) $(1A) $(2Ai) $(2Aii) $(3A) \
		$(4A1) $(4A2) $(4A3) $(4A4) $(4A5) $(4A6) $(4A7) \
		$(1B1) $(1B2) $(2B) $(3B)

# Clean
clean:
	rm -rfv figures/ outputs/ data-cleaned/

# Generate data
data-cleaned/SPARS_A.csv data-cleaned/SPARS_A.rds: \
0A-clean-data.R data/*.xlsx
	Rscript "$<"

data-cleaned/SPARS_B.csv data-cleaned/SPARS_B.rds: \
0B-clean-data.R data/*.txt
	Rscript "$<"

# Generate outputs
outputs/1A-participant-descriptive-data.html outputs/1A-participant-descriptive-data.md: \
1A-participant-descriptive-data.Rmd \
data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/1A-participant-descriptive-data outputs/figures/

outputs/suppl_01_3B-instructions-for-participants.html: \
suppl_01_3B-instructions-for-participants.Rmd \
data/spars.png \
data/nrs.png \
data/srs.png
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"

outputs/suppl_02_2A-central-tendency-summary.html outputs/suppl_02_2A-central-tendency-summary.md: \
suppl_02_2A-central-tendency-summary.Rmd \
data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"

outputs/suppl_03_2A-central-tendency.html outputs/suppl_03_2A-central-tendency.md: \
suppl_03_2A-central-tendency.Rmd \
data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl_03_2A-central-tendency outputs/figures/

outputs/suppl_04_3A-order-effects.html outputs/suppl_04_3A-order-effects.md: \
suppl_04_3A-order-effects.Rmd \
data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl_04_3A-order-effects outputs/figures/

outputs/suppl_05_4A-stimulus-response-1.html outputs/suppl_05_4A-stimulus-response-1.md: \
suppl_05_4A-stimulus-response-1.Rmd \
data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl_05_4A-stimulus-response-1 outputs/figures/

outputs/suppl_06_4A-stimulus-response-2.html outputs/suppl_06_4A-stimulus-response-2.md: \
suppl_06_4A-stimulus-response-2.Rmd \
data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl_06_4A-stimulus-response-2 outputs/figures/

outputs/suppl_07_4A-stimulus-response-3.html outputs/suppl_07_4A-stimulus-response-3.md: \
suppl_07_4A-stimulus-response-3.Rmd \
data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl_07_4A-stimulus-response-3 outputs/figures/

outputs/suppl_08_4A-stimulus-response-4.html outputs/suppl_08_4A-stimulus-response-4.md: \
suppl_08_4A-stimulus-response-4.Rmd \
data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl_08_4A-stimulus-response-4 outputs/figures/

outputs/suppl_09_4A-stimulus-response-5.html outputs/suppl_09_4A-stimulus-response-5.md: \
suppl_09_4A-stimulus-response-5.Rmd \
data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl_09_4A-stimulus-response-5 outputs/figures/

outputs/suppl_10_4A-stimulus-response-6.html outputs/suppl_10_4A-stimulus-response-6.md: \
suppl_10_4A-stimulus-response-6.Rmd \
data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl_10_4A-stimulus-response-6 outputs/figures/

outputs/suppl_11_1B-stimulus-response-1.html outputs/suppl_11_1B-stimulus-response-1.md: \
suppl_11_1B-stimulus-response-1.Rmd \
data-cleaned/SPARS_B.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl_11_1B-stimulus-response-1 outputs/figures/

outputs/suppl_12_1B-stimulus-response-2.html outputs/suppl_12_1B-stimulus-response-2.md: \
suppl_12_1B-stimulus-response-2.Rmd \
data-cleaned/SPARS_B.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl_12_1B-stimulus-response-2 outputs/figures/

outputs/suppl_13_2B-scale-agreement.html outputs/suppl_13_2B-scale-agreement.md: \
suppl_13_2B-scale-agreement.Rmd \
data-cleaned/SPARS_B.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl_13_2B-scale-agreement outputs/figures/

outputs/suppl_14_4A-figure-4-new.html outputs/suppl_14_4A-figure-4-new.md: \
suppl_14_4A-figure-4-new.Rmd \
data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/suppl_14_4A-figure-4-new outputs/figures/
