
drv <- dbDriver("Oracle")
con <- dbConnect(drv, "PRUDEV", Sys.getenv("PRUDEV_psw"), dbname=Sys.getenv("DB"))
STATFOR_data=dbGetQuery(con, paste0("select  
to_char(f.lobt,'YYYY') as YEAR
, EXTRACT (MONTH FROM f.lobt) MONTH_NUM
, to_char(f.lobt,'MON') as MONTH_MON 
, lobt
, c.tz_name
, daio
, r.sk_flt_type_rule_id 
, r.RULE_NAME 
, sum(tf_tz) as FLIGHT
, sum(state_dur)/60 as DUR_HRS
, sum(f.act_dist)*1.852 as DIST_KM
from  swh_dm.dm_tz_fir_m4  f  ,  swh_fct.dimcl_tz c , SWH_FCT.DIM_FLIGHT_TYPE_RULE r
where f.lobt >= '01-JAN-2008'
      and c.SK_T2TR_ID = f.sk_dimcl_tz_id
      and R.SK_FLT_TYPE_RULE_ID = F.SK_FLT_TYPE_RULE_ID 
      --and c.tz_name = 'ESRA08'
      and (c.tr_name like 'ESRA%' or c.tr_name like 'Other Europe' or c.tr_name like 'TR-ESRA 2008' or c.tr_name like 'TR-ECAC')   
group by c.tz_name , lobt, daio , r.sk_flt_type_rule_id, R.RULE_NAME order by lobt"))
PRU_FAC_data=dbGetQuery(con, paste0("SELECT * 
                                     FROM PRUDEV.V_PRU_FAC_TDC_DD 
                                     WHERE UNIT_PRU_TYPE in ('ANSP','ZONE', 'ZONE_ANSP') AND ENTRY_DATE >= '1-JAN-2008'"))
dbDisconnect(con)
