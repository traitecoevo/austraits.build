
# Use google drive to upload files for distribution via email and share with link
# this script assumes you running it from within your local copy of Austraits

upload_report <- function(dataset_id, overwrite = FALSE, austraits_path_zip = "export/zip_reviews") {

	# check relevant path exists locally
	dir.create(austraits_path_zip, FALSE, TRUE)

	# files for upload
	file_zip <- paste0(dataset_id, "-review-", stringr::str_replace_all(Sys.Date(), "-", "_"), ".zip")

	# set path in google drive
	drive_path <- file.path("austraits/data reviews/dataset_id", dataset_id)

	# Create folder on drive if needed
	drive_id <- googledrive::drive_get(drive_path) 
	if(nrow(drive_id)==0)
	 	googledrive::drive_mkdir(drive_path, overwrite = FALSE)

	# check if report file already exists
	drive_id <- googledrive::drive_get(file.path(drive_path, file_zip))

	if(nrow(drive_id) > 0 & !overwrite) {
			message("Using existing file")
		}

	if(nrow(drive_id) > 0 & overwrite) {
			message("Removing existing report for this date")
			drive_rm(as_id(drive_id))
		}

	# Upload report
	if(nrow(drive_id) == 0 | overwrite) {
		# prepare files for upload
		files <- c(
	          file.path("export/reports", paste0(dataset_id, ".html")),
	         	file.path("data", dataset_id, "data.csv"), 
	         	file.path("data", dataset_id, "metadata.yml"))
		if(any(!file.exists(files))) {
			stop(sprintf("File %s does not exist", files[!file.exists(files)]))
		}

		file_zip_source <- file.path(austraits_path_zip, file_zip)
		zip::zipr(file_zip_source, files)

		# upload zip to google drive
		drive_id <-	googledrive::drive_upload(file_zip_source, 
		                 name = file_zip, path = drive_path)
	}

	# share file
	drive_id <- googledrive::as_id(drive_id)

	googledrive::drive_share(drive_id, role = c("reader"), type = c("anyone"), verbose = TRUE)

	list(dataset_id = dataset_id,
	     id = drive_id, 
	     link = sprintf("https://drive.google.com/uc?id=%s&export=download", drive_id) 
	   	# Could also use sprintf("https://drive.google.com/uc?id=%s", tmp)
	     )
}

review_email_text1 <- function(out, name="XXX") {

paste0('<html><style type="text/css">html{font-family:sans-serif; font-size:12}body{margin:10}</style><body>

<p>Dear ', name,',</p>
<p><strong>We are delighted to invite you to co-author a paper about AusTraits: an open source compilation of data on the traits of Australian plant species.</strong> Including your contributions, AusTraits currently includes 325,819 trait-by-species combinations across 20,321 species, from 148 primary sources, including published papers, theses, floras, taxonomic treatments, and unpublished sources. Our plan from here is to submit a data paper describing the dataset to the journal  <a href="https://www.nature.com/sdata">"Scientific Data"</a>, with all data contributors invited as co-authors. At that time, the dataset will also be made open to the public for re-use and discovery.</p>
<p>We are contacting you now regarding a dataset included in AusTraits under the name "', out$dataset_id,'". We have you listed as the lead contact for that dataset. The information included in this email provides you opportunity to</p>
<ul>
<li>confirm your willingness to include the data in AusTraits,</li>
<li>review the data you have contributed,</li>
<li>provide corrections where needed,</li>
<li>confirm your desire to be a co-author on the paper.</li>
</ul>
<p>At the following links, you will find</p>
<ol type="1">
<li>A draft of the paper for submission to Scientific Data <a href="https://drive.google.com/open?id=1Sn5q0jJuz1qkYQ4nAdO8D2xK8XofNVJ1">link</a></li>
<li>A copy of the raw data we have from "', out$dataset_id,'" and a report of the data, outlining the contents of the data, including source, metadata, and plots comparing your data to the rest of AusTraits: <a href="', out$link, '">link</a>. (<strong> NB: Clicking on this link will download material to your machine. </strong>)</li>
</ol>
<p><strong>What we ask now is that you download and review these materials. Please enter your responses into the response form <a href="https://forms.gle/vhZgxDd734dDddG88">here</a> within 4 weeks, ie. by June 15th.</strong> (If you can reply sooner that would be awesome, as we can start processing your response).</p>
<p>The time-line for AusTraits going forward is as follows:</p>
<ul>
<li>21 May: Send reports and papers to contributors for review, start processing responses</li>
<li>15 June: Close time for receiving reports</li>
<li>30 June: Circulate final draft to co-authors</li>
<li>20 July: Post draft paper as preprint on Eco-Evo Arxiv, submit to Scientific Data</li>
<li>30 July: Open call for new data submissions. New contributors will be invited co authors on the publication before final submission</li>
<li>Aug-Sep: Processing new data submissions</li>
<li>Oct: Resubmit paper to Scientific Data</li>
</ul>
<p>We\'re delighted to have you involved and look forward to hearing from you!</p>
<p>With best wishes, </p>
<p> Current AusTraits team (Rachael Gallagher & Daniel Falster - strategic & technical leads, Elizabeth Wenk & Caitlan Baxter - data processing leads)</p>

</body></html>'
) %>% paste(collapse = "\n")

}

review_email_text2 <- function(out, name="XXX") {
  
  paste0('<html><style type="text/css">html{font-family:sans-serif; font-size:12}body{margin:10}</style><body>

<p>Dear ', name,',</p>
<p><strong>Thank you for contributing your dataset to AusTraits, an open source, harmonized compilation of trait data for Australian plants.
As we discussed, by contributing your data, you are being invited to be a co-author on a paper about AusTraits.
We have merged your dataset into the AusTraits framework under the name "', out$dataset_id,'".
We have generated a report summarising the information contained in your dataset and accompanying metadata file.
We have you listed as the lead contact for that dataset. The information included in this email provides you opportunity to</p>
<ul>
<li>confirm your willingness to include the data in AusTraits,</li>
<li>review the data you have contributed,</li>
<li>provide corrections where needed,</li>
<li>confirm your desire to be a co-author on the paper.</li>
</ul>
<p>In your report there are many auto-generated questions scattered through the text. 
Do not feel the need to address all of them - they are prompts to ensure you look carefully at the data summaries and confirm
that you have provided us with all relevant data for the study. 
Most important to the future usefulness of AusTraits is that you provide location data (if available and relevant), 
that you confirm numeric data values fall where you expect them to, and that you can confirm the data correspond to the reference(s) listed.
<p>At the following links, you will find</p>
<ol type="1">
<li>A draft of the paper for submission to Scientific Data <a href="https://drive.google.com/open?id=1Sn5q0jJuz1qkYQ4nAdO8D2xK8XofNVJ1">link</a></li>
<li>A copy of the raw data we have from "', out$dataset_id,'" and a report of the data, outlining the contents of the data, including source, metadata, and plots comparing your data to the rest of AusTraits: <a href="', out$link, '">link</a>. (<strong> NB: Clicking on this link will download material to your machine. </strong>)</li>
</ol>
<p><strong>What we ask now is that you download and review these materials. Please enter your responses into the response form <a href="https://forms.gle/vhZgxDd734dDddG88">here</a> by November 15th.</strong> (If you can reply sooner that would be awesome, as we can start processing your response).</p>
<p>We plan to release AusTraits and submit the data paper by the end of 2019.

</ul>
<p>We\'re delighted to have you involved and look forward to hearing from you!</p>
<p>With best wishes, </p>
<p> Current AusTraits team (Rachael Gallagher & Daniel Falster - strategic & technical leads, Elizabeth Wenk & Caitlan Baxter - data processing leads)</p>

</body></html>'
  ) %>% paste(collapse = "\n")
  
}
