
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


con2 <- dbConnect(drv, "PRU_CPLX", Sys.getenv("PRU_CPLX_psw"), dbname=Sys.getenv("DB"))
CPLX_data=dbGetQuery(con, paste0("
SELECT TO_CHAR (t.cplx_date, 'YYYY') year,
EXTRACT (MONTH FROM t.cplx_date) MONTH_NUM,
TO_CHAR (t.cplx_date, 'MON') MONTH_MON,
cplx_date as FLT_DATE,
u.ansp_name ENTITY_NAME,
fl,
'ANSP (AUA)' ENTITY_TYPE,
SUM (ft)/12 CPLX_FLIGHT_HRS,
SUM (tx)/12 CPLX_INTER_HRS,
SUM (txv)/12 VERTICAL_INTER_HRS,
SUM (txh)/12 HORIZ_INTER_HRS,
SUM (txs)/12 SPEED_INTER_HRS, 
'3.13.1' as BADA_VERSION
FROM aua_complexity_3_13 t, prudev.V_PRU_REL_CFMU_AUA_ANSP u
WHERE     t.unit_code = u.aua_code
AND fl >= 100
AND t.cplx_date BETWEEN u.wef AND u.till
AND u.ANSP_ACE_CODE IS NOT NULL
and t.cplx_date >='01-jan-2017'        
--  AND u.pru_atc_type <> 'OAC'
--  AND u.ansp_name NOT IN ('MILITARY', 'AIRPORT', 'UNKNOWN')
GROUP BY TO_CHAR (t.cplx_date, 'YYYY'),
EXTRACT (MONTH FROM t.cplx_date),
TO_CHAR (t.cplx_date, 'MON'),
cplx_date,
u.ansp_name, fl
ORDER BY 4"))
dbDisconnect(con2)
