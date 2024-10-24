package com.autel.cloud.pile.base.domain.common;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Author:   A19011
 * Description: CountryCodeCacheRunner
 * Date:     2022/12/16 15:27
 *
 * @Version 0.0.1-SNAPSHOT
 */

@Slf4j
@Component
public class CountryCodeCacheRunner implements CommandLineRunner {

    public static Map<String, String> countryCodeMap = new ConcurrentHashMap<>();

    @Override
    public void run(String... args) throws Exception {
        countryCodeMap.clear();
        log.info("--------------------CoutryCode缓存开始-------------------");
        String countryCodes = "AW,ABW@" +
                "AF,AFG@" +
                "AO,AGO@" +
                "AI,AIA@" +
                "AX,ALA@" +
                "AL,ALB@" +
                "AD,AND@" +
                "AE,ARE@" +
                "AR,ARG@" +
                "AM,ARM@" +
                "AS,ASM@" +
                "AQ,ATA@" +
                "TF,ATF@" +
                "AG,ATG@" +
                "AU,AUS@" +
                "AT,AUT@" +
                "AZ,AZE@" +
                "BI,BDI@" +
                "BE,BEL@" +
                "BJ,BEN@" +
                "BQ,BES@" +
                "BF,BFA@" +
                "BD,BGD@" +
                "BG,BGR@" +
                "BH,BHR@" +
                "BS,BHS@" +
                "BA,BIH@" +
                "BL,BLM@" +
                "BY,BLR@" +
                "BZ,BLZ@" +
                "BM,BMU@" +
                "BO,BOL@" +
                "BR,BRA@" +
                "BB,BRB@" +
                "BN,BRN@" +
                "BT,BTN@" +
                "BV,BVT@" +
                "BW,BWA@" +
                "CF,CAF@" +
                "CA,CAN@" +
                "CC,CCK@" +
                "CH,CHE@" +
                "CL,CHL@" +
                "CN,CHN@" +
                "CI,CIV@" +
                "CM,CMR@" +
                "CD,COD@" +
                "CG,COG@" +
                "CK,COK@" +
                "CO,COL@" +
                "KM,COM@" +
                "CV,CPV@" +
                "CR,CRI@" +
                "CU,CUB@" +
                "CW,CUW@" +
                "CX,CXR@" +
                "KY,CYM@" +
                "CY,CYP@" +
                "CZ,CZE@" +
                "DE,DEU@" +
                "DJ,DJI@" +
                "DM,DMA@" +
                "DK,DNK@" +
                "DO,DOM@" +
                "DZ,DZA@" +
                "EC,ECU@" +
                "EG,EGY@" +
                "ER,ERI@" +
                "EH,ESH@" +
                "ES,ESP@" +
                "EE,EST@" +
                "ET,ETH@" +
                "FI,FIN@" +
                "FJ,FJI@" +
                "FK,FLK@" +
                "FR,FRA@" +
                "FO,FRO@" +
                "FM,FSM@" +
                "GA,GAB@" +
                "GB,GBR@" +
                "GE,GEO@" +
                "GG,GGY@" +
                "GH,GHA@" +
                "GI,GIB@" +
                "GN,GIN@" +
                "GP,GLP@" +
                "GM,GMB@" +
                "GW,GNB@" +
                "GQ,GNQ@" +
                "GR,GRC@" +
                "GD,GRD@" +
                "GL,GRL@" +
                "GT,GTM@" +
                "GF,GUF@" +
                "GU,GUM@" +
                "GY,GUY@" +
                "HK,HKG@" +
                "HM,HMD@" +
                "HN,HND@" +
                "HR,HRV@" +
                "HT,HTI@" +
                "HU,HUN@" +
                "ID,IDN@" +
                "IM,IMN@" +
                "IN,IND@" +
                "IO,IOT@" +
                "IE,IRL@" +
                "IR,IRN@" +
                "IQ,IRQ@" +
                "IS,ISL@" +
                "IL,ISR@" +
                "IT,ITA@" +
                "JM,JAM@" +
                "JE,JEY@" +
                "JO,JOR@" +
                "JP,JPN@" +
                "KZ,KAZ@" +
                "KE,KEN@" +
                "KG,KGZ@" +
                "KH,KHM@" +
                "KI,KIR@" +
                "KN,KNA@" +
                "KR,KOR@" +
                "KW,KWT@" +
                "LA,LAO@" +
                "LB,LBN@" +
                "LR,LBR@" +
                "LY,LBY@" +
                "LC,LCA@" +
                "LI,LIE@" +
                "LK,LKA@" +
                "LS,LSO@" +
                "LT,LTU@" +
                "LU,LUX@" +
                "LV,LVA@" +
                "MO,MAC@" +
                "MF,MAF@" +
                "MA,MAR@" +
                "MC,MCO@" +
                "MD,MDA@" +
                "MG,MDG@" +
                "MV,MDV@" +
                "MX,MEX@" +
                "MH,MHL@" +
                "MK,MKD@" +
                "ML,MLI@" +
                "MT,MLT@" +
                "MM,MMR@" +
                "ME,MNE@" +
                "MN,MNG@" +
                "MP,MNP@" +
                "MZ,MOZ@" +
                "MR,MRT@" +
                "MS,MSR@" +
                "MQ,MTQ@" +
                "MU,MUS@" +
                "MW,MWI@" +
                "MY,MYS@" +
                "YT,MYT@" +
                "NA,NAM@" +
                "NC,NCL@" +
                "NE,NER@" +
                "NF,NFK@" +
                "NG,NGA@" +
                "NI,NIC@" +
                "NU,NIU@" +
                "NL,NLD@" +
                "NO,NOR@" +
                "NP,NPL@" +
                "NR,NRU@" +
                "NZ,NZL@" +
                "OM,OMN@" +
                "PK,PAK@" +
                "PA,PAN@" +
                "PN,PCN@" +
                "PE,PER@" +
                "PH,PHL@" +
                "PW,PLW@" +
                "PG,PNG@" +
                "PL,POL@" +
                "PR,PRI@" +
                "KP,PRK@" +
                "PT,PRT@" +
                "PY,PRY@" +
                "PS,PSE@" +
                "PF,PYF@" +
                "QA,QAT@" +
                "RE,REU@" +
                "RO,ROU@" +
                "RU,RUS@" +
                "RW,RWA@" +
                "SA,SAU@" +
                "SD,SDN@" +
                "SN,SEN@" +
                "SG,SGP@" +
                "GS,SGS@" +
                "SH,SHN@" +
                "SJ,SJM@" +
                "SB,SLB@" +
                "SL,SLE@" +
                "SV,SLV@" +
                "SM,SMR@" +
                "SO,SOM@" +
                "PM,SPM@" +
                "RS,SRB@" +
                "SS,SSD@" +
                "ST,STP@" +
                "SR,SUR@" +
                "SK,SVK@" +
                "SI,SVN@" +
                "SE,SWE@" +
                "SZ,SWZ@" +
                "SX,SXM@" +
                "SC,SYC@" +
                "SY,SYR@" +
                "TC,TCA@" +
                "TD,TCD@" +
                "TG,TGO@" +
                "TH,THA@" +
                "TJ,TJK@" +
                "TK,TKL@" +
                "TM,TKM@" +
                "TL,TLS@" +
                "TO,TON@" +
                "TT,TTO@" +
                "TN,TUN@" +
                "TR,TUR@" +
                "TV,TUV@" +
                "TW,TWN@" +
                "TZ,TZA@" +
                "UG,UGA@" +
                "UA,UKR@" +
                "UM,UMI@" +
                "UY,URY@" +
                "US,USA@" +
                "UZ,UZB@" +
                "VA,VAT@" +
                "VC,VCT@" +
                "VE,VEN@" +
                "VG,VGB@" +
                "VI,VIR@" +
                "VN,VNM@" +
                "VU,VUT@" +
                "WF,WLF@" +
                "WS,WSM@" +
                "YE,YEM@" +
                "ZA,ZAF@" +
                "ZM,ZMB@" +
                "ZW,ZWE";

        String[] countrCodeArray = countryCodes.split("@");
        for(String code : countrCodeArray ){
            String[] codeArray = code.split(",");
            countryCodeMap.put(codeArray[0], codeArray[1]);
        }

        log.info("--------------------CoutryCode缓存完成-------------------", countryCodeMap.size());
    }
}
