library(HandyCode)
library(plyr)
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/r4ss")
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/nwfscDiag")
mydir = "C:/Users/Chantel.Wetzel/Documents/GitHub/nwfscDiag/example"
model_settings = get_settings(settings = list(base_name = "simple",
											  Njitter = 2, 
											  run = c("jitter", "profile", "retro"),
    										  profile_para = c("female_m", "h", "r0"),
											  retro_yrs = -1:-2))

run_diagnostics(mydir = mydir, model_settings = model_settings)


model_settings = get_settings(settings = list(base_name = "simple",
											  run = "profile",
											  profile_para = NULL,
											  profile_custom = "L_at_Amin_Fem_GP_1",
											  para_custom_range = c(17, 24, 1),
											  #para_range_h = c(0.50, 0.75, 0.05),
											  Njitter = 2, 
											  retro_yrs = -1:-2))
run_diagnostics(mydir = mydir, model_settings = model_settings)
