
drv <- dbDriver("Oracle")
con <- dbConnect(drv, "PRUDEV", Sys.getenv("PRUDEV_psw"), dbname=Sys.getenv("DB"))
Flight_data=dbGetQuery(con, paste0("select  
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
PRU_APT_ATFM_delay=dbGetQuery(con, "SELECT 
to_char(FLIGHT_DATE,'YYYY') as YEAR,
EXTRACT (MONTH FROM FLIGHT_DATE) MONTH_NUM,  
to_char(FLIGHT_DATE,'MON') as MONTH_MON,
FLIGHT_DATE as FLT_DATE,   
ARP_CODE as APT_ICAO,
PRU_APT_NAME as APT_NAME, 
PRU_State_NAME as STATE_NAME,  
TTF_ARR as FLT_ARR_1,
TDM_ARP_ARR as DLY_APT_ARR_1,
TDM_ARP_ARR_A as DLY_APT_ARR_A_1, 
TDM_ARP_ARR_C as DLY_APT_ARR_C_1, 
TDM_ARP_ARR_D as DLY_APT_ARR_D_1, 
TDM_ARP_ARR_E as DLY_APT_ARR_E_1,
TDM_ARP_ARR_G as DLY_APT_ARR_G_1, 
TDM_ARP_ARR_I as DLY_APT_ARR_I_1, 
TDM_ARP_ARR_M as DLY_APT_ARR_M_1, 
TDM_ARP_ARR_N as DLY_APT_ARR_N_1, 
TDM_ARP_ARR_O as DLY_APT_ARR_O_1,
TDM_ARP_ARR_P as DLY_APT_ARR_P_1, 
TDM_ARP_ARR_R as DLY_APT_ARR_R_1, 
TDM_ARP_ARR_S as DLY_APT_ARR_S_1, 
TDM_ARP_ARR_T as DLY_APT_ARR_T_1, 
TDM_ARP_ARR_V as DLY_APT_ARR_V_1, 
TDM_ARP_ARR_W as DLY_APT_ARR_W_1,
TDM_ARP_ARR_NA as DLY_APT_ARR_NA_1,
TDF_ARP_ARR as FLT_ARR_1_DLY,
TDF_15_ARP_ARR as FLT_ARR_1_DLY_15,
ATFM_Version
--CASE WHEN FLIGHT_DATE<'04-APR-2016' THEN 'v1' ELSE 'v1' END as Version 
FROM PRUDEV.V_PRU_FAC_TDC_ARP_ARR_DD a, PRUDEV.DSH_REL_AIRPORT_COUNTRY b
WHERE FLIGHT_DATE >= '1-JAN-2011'
AND a.ARP_code = b.apt_icao
AND ARP_CODE IN (SELECT apt_icao FROM PRUDEV.DSH_REL_AIRPORT_COUNTRY)
order by 4")
dbDisconnect(con)


con2 <- dbConnect(drv, "PRU_CPLX", Sys.getenv("PRU_CPLX_psw"), dbname=Sys.getenv("DB"))
CPLX_data=dbGetQuery(con2, paste0("
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

con3 <- dbConnect(drv, "PRUTEST", Sys.getenv("PRUTEST_psw"), dbname=Sys.getenv("DB"))
HFE_data=dbGetQuery(con3, "With SRC as (select a.*, b.*
FROM PRUTEST.HFE_DAILY a, PRUTEST.DSH_BRIDGE_NAMES b
WHERE entry_date >= '01-JAN-2015'and a.mes_area = b.mes_area)
SELECT 
         EXTRACT (YEAR FROM entry_date) YEAR
,        EXTRACT (MONTH FROM entry_date) MONTH_NUM
,        TO_CHAR (entry_date, 'MON') MONTH_MON
,        entry_date
,        ENTITY_NAME
,        ENTITY_TYPE
,        model_type AS TYPE_MODEL
,        flown_km AS DIST_FLOWN_KM
,        round(direct_km, 2) AS DIST_DIRECT_KM
,        round(achieved_km, 2) AS DIST_ACHIEVED_KM
,        FLIGHTS AS FLIGHTS
FROM SRC")
dbDisconnect(con3)

CDO_CCO_data=read.xlsx("https://coll.eurocontrol.int/sites/pru/dashboard/Data/Vertical_Flight_Efficiency_cdo_cco.xlsx", sheet = "DATA")


