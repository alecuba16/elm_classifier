library(RMySQL)
source('functions_common/db_query.R')
db_config_data <- data.frame(user='user',
                       password='password',
                       dbname='yourHistoricalBD',
                       host='yourHost',
                       port=3306)

automatic_host <- TRUE
if(automatic_host && !(Sys.info()["nodename"] %in% c("smartbbdd")))
    db_config_data$host <- '127.0.0.1'

wp_id <- 20 # 21
wp_code <- "izco" # "moncay"
array_ot <- "10068" # "10057,10066" # "10065,10067,10068,10069"
fault <- "gbox2_u" # "gen2_u"
#exclude_pattern <- "^CmdWTG|^Cont|^Frec|^Grd|^IndTurb|^Top|^VelGen|^VelPitch|^SobreVelMec"
exclude_variables <- "^CmdWTG|^Cont|^Frec|^Grd|^IndTurb|^Top|^VelGen|^VelPitch|^SobreVelMec"
query <- paste0("SELECT ld_id, ld_code, wp_id FROM smartcast_DB.SC_LOGICALDEVICE WHERE wp_id = ",wp_id)
model_dic <- db_query(query, db_config = db_config_data)$data

for(i in 1:nrow(model_dic) ) {
    # query <- paste0("INSERT INTO `yourHistoricalBD`.`1_cast_config_test` (`wp_id`, `wp_code`, `ld_id`, `ld_code`, `fault`, `array_ot`, `type`, `filter`, `power_condition`, `exclude_variables`, `target`, `creation_date_ini`, `creation_date_end`, `creation_wtdata_date_ini`, `creation_wtdata_date_end`, `creation_model_path`, `creation_log_path`, `update_log_path`, `cast_enable`, `cast_log_path`) VALUES (",wp_id,", '",wp_code,"', ",model_dic$ld_id[i],", '",model_dic$ld_code[i],"', '",fault,"', '",array_ot,"', 'phealth', 'f8sd', '>0', 'model,fake_data,alarm_block_code,alarm_all,alarm_all_block_code,ot_block_code,ot_all,ot_all_block_code,alarm', '",exclude_pattern,"', 'ot', NULL, NULL, '2014-01-01 00:00:00', '2014-12-31 23:59:59', 'windfarms/', '', 'windfarms/', 1, 'windfarms/');")
    query <- paste0("INSERT INTO `schistorical_db`.`1_cast_config_park_izco` (`wp_id`, `wp_code`, `fault`, `target`, `array_ot`, `array_id_ot_in`, `type`, `include_variables`, `exclude_variables`, `creation_enable`, `creation_trn_percent`, `ld_id`, `ld_code`, `creation_wtdata_date_ini`, `creation_wtdata_date_end`, `creation_model_path`, `creation_date_ini`, `creation_date_end`, `creation_log_path`, `creation_error`, `report_enable`) VALUES (20, 'izco', 'gbox2_u', 'ot', '10068', NULL, 'phealth', NULL, 'model,fake_data,alarm_block_code,alarm_all,alarm_all_block_code,ot_block_code,ot_all,ot_all_block_code,alarm', '^CmdWTG|^Cont|^Frec|^Grd|^IndTurb|^Top|^VelGen|^VelPitch|^SobreVelMec|^Tens', 0, 70, ",model_dic$ld_id[i],", '",model_dic$ld_code[i],"', '2014-01-01 00:00:00', '2016-01-01 00:00:00', 'windfarms/izco/park_models/2017-09-14-15-29_gbox2_u_f8sd.RData.gz', '2017-09-14 15:29:04', '2017-09-14 15:44:37', 'windfarms/', '0', '1');")
    cat(query,"\n")
    r <- db_query(query, db_config = db_config_data)
    cat("\t",r$msg,"\n")
}
