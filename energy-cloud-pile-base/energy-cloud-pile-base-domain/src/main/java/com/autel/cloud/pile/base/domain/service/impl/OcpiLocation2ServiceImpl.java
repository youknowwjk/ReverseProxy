package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.util.DateUtil;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.pile.base.domain.common.CountryCodeCacheRunner;
import com.autel.cloud.pile.base.domain.service.OcpiLocation2Service;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseService;
import com.autel.cloud.pile.base.domain.service.OpLocationPileEvseService;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationConnectorMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationEvseMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationOperationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationConnectorEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEvseEntity;
import com.autel.cloud.pile.base.vo.HoursVO;
import com.autel.cloud.pile.base.vo.OcpiLocationMappingVO;
import com.autel.cloud.pile.base.vo.ocpi.v2.OcpiV2ConnectorVO;
import com.autel.cloud.pile.base.vo.ocpi.v2.OcpiV2EvseVO;
import com.autel.cloud.pile.base.vo.ocpi.v2.OcpiV2LocationVO;
import com.autel.cloud.pile.bill.enums.RoamingProtocolEnum;
import com.autel.cloud.pile.bill.feign.IBillFeignClient;
import com.autel.cloud.pile.bill.vo.GeoLocation;
import com.autel.cloud.pile.bill.vo.RoamingRulesVO;
import com.autel.cloud.tariff.feign.OcpiTariffFeignClient;
import com.autel.cloud.tariff.feign.TariffFeignClient;
import com.autel.cloud.tariff.vo.ocpi.TariffVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * Author:   A19011
 * Description: OcpiLocationServiceImpl
 * Date:     2022/6/20 15:49
 *
 * @Version 0.0.1-SNAPSHOT
 */
@Service
@Slf4j
public class OcpiLocation2ServiceImpl implements OcpiLocation2Service {
    @Autowired
    OpLocationMapper opLocationMapper;
    @Autowired
    OpLocationEvseMapper opLocationEvseMapper;
    @Autowired
    OpLocationConnectorMapper opLocationConnectorMapper;
    @Autowired
    OpLocationOperationMapper opLocationOperationMapper;
    @Autowired
    MonitorFeignClient monitorFeignClient;
    @Value("${ocpi.page.link}")
    private String pageLinkFormat;
    @Value("${ocpi.location-type}")
    private String locationType;
    @Value("${ocpi.location-country}")
    private String locationCountry;
    @Value("${ocpi.location-city}")
    private String locationCity;
    @Value("${ocpi.connector-standard}")
    private String connectorStandard;
    @Value("#{'${ocpi.evse-capabilities}'.split(',')}")
    private List<String> evseCapabilities;
    @Value("${ocpi.country-code}")
    private String countryCode;
    @Value("${ocpi.party-id}")
    private String partyId;
    @Autowired
    private TariffFeignClient tariffFeignClient;

    @Autowired
    private OpLocationService opLocationService;

    @Autowired
    private OpLocationPileEvseService opLocationPileEvseService;

    @Autowired
    private OpLocationEvseService opLocationEvseService;

    @Autowired
    private OcpiTariffFeignClient ocpiTariffFeignClient;

    @Autowired
    private IBillFeignClient billFeignClient;


    @Override
    public ResponseEntity<List<OcpiV2LocationVO>> getLocationList(String dateFrom, String dateTo, Integer offset, Integer limit, String locationLink, String cpoId) {
        log.info("参数校验dateFrom：{}， dateTo：{}, cpoId:{}",dateFrom,dateTo, cpoId);
        if(Boolean.TRUE.equals((!DateUtil.validateDate(dateFrom)) || !DateUtil.validateDate(dateTo))){
            return ResponseEntity.badRequest().build();
        }

        List<String> operatorIdList = new ArrayList<>();
        List<String> excludeOperatorIdList = new ArrayList<>();
        if(StringUtils.isNotBlank(cpoId)){
            List<RoamingRulesVO> roamingRulesVOList = billFeignClient.getSellerByIndependentCpoId(cpoId, RoamingProtocolEnum.OCPI.getCode()).getData();
            log.info("operatorIdList roamingRulesVOList :{}", JSON.toJSONString(roamingRulesVOList));
            if(!CollectionUtils.isEmpty(roamingRulesVOList)){
                roamingRulesVOList.forEach(r->{
                    operatorIdList.add(r.getCpoId());
                });
            }
        }

        if(CollectionUtils.isEmpty(operatorIdList)){
            List<RoamingRulesVO> roamingRulesVOList = billFeignClient.getSellerIdsByCondition(RoamingProtocolEnum.OCPI.getCode(), true).getData();
            log.info("excludeOperatorIdList roamingRulesVOList :{}", JSON.toJSONString(roamingRulesVOList));
            roamingRulesVOList.forEach(r->{
                excludeOperatorIdList.add(r.getCpoId());
            });
        }

        log.info("getLocationList operatorIdList :{}, excludeOperatorIdList:{}",
                JSON.toJSONString(operatorIdList),
                JSON.toJSONString(excludeOperatorIdList));

        Integer totalCount = getTotalCount(dateFrom,dateTo, operatorIdList, excludeOperatorIdList);
        List<OcpiV2LocationVO> result = new ArrayList<>();
        log.info("分页获取场站参数{},{},{},{}",dateFrom,dateTo,offset,limit);
        List<OpLocationEntity>  locationList = getPageStation(dateFrom,dateTo,offset,limit, operatorIdList, excludeOperatorIdList);
        log.info("分页获取场站当前页记录数{}",locationList.size());

        if(!CollectionUtils.isEmpty(locationList)){
            for(OpLocationEntity location : locationList){
                OcpiLocationMappingVO locationMappingVO = convertOcpiLocationMappingVO(location);
                result.add(getLocationVO(locationMappingVO, null));
            }
        }

        boolean isLastPage = totalCount-offset <= limit;
        if(!isLastPage){
            offset = offset + limit;
            String link = String.format(pageLinkFormat, locationLink, StringUtils.defaultString(dateFrom,""),
                    StringUtils.defaultString(dateTo,""), limit, offset);
            link = link.replace("date_from=&","");
            link = link.replace("date_to=&","");
            return ResponseEntity.ok().header("X-Total-Count", totalCount+"")
                    .header("X-Limit", limit+"")
                    .header("Link", link)
                    .body(result);
        }

        return ResponseEntity.ok().header("X-Total-Count", totalCount+"")
                .header("X-Limit", limit+"")
                .body(result);
    }

    @Override
    public OcpiV2LocationVO getEvseLocation(String locationId, String evseUid) {
        OpLocationEntity location = opLocationMapper.selectById(locationId);
        if(location == null || location.getId() == null || location.getHubjectCheck() == null || !location.getHubjectCheck()){
            return null;
        }
        OcpiLocationMappingVO locationMappingVO = convertOcpiLocationMappingVO(location);
        log.info("locationMappingVO :{}", JSON.toJSONString(locationMappingVO));
        return getLocationVO(locationMappingVO, evseUid);
    }


    /**
     * 分页获取DB场站数据
     * @param dateFrom
     * @param dateTo
     * @param operatorIdList
     * @return
     */
    private Integer getTotalCount(String dateFrom, String dateTo, List<String> operatorIdList, List<String> excludeOperatorIdList){
        QueryWrapper<OpLocationEntity> queryWrapper=new QueryWrapper<>();
        queryWrapper.eq("status", 1);
        queryWrapper.eq("deleted", 0);
        queryWrapper.eq("hubject_check", 1);
        if(!CollectionUtils.isEmpty(operatorIdList)){
            queryWrapper.in("operator_id", operatorIdList);
        }
        if(!CollectionUtils.isEmpty(excludeOperatorIdList)){
            queryWrapper.notIn("operator_id", excludeOperatorIdList);
        }

        if(StringUtils.isNotBlank(dateFrom)){
            queryWrapper.apply("IFNULL(updated_at,created_at) >= 1000 * UNIX_TIMESTAMP('" + DateUtil.formatUtcYYYYMMDDHHmmss(dateFrom) + "')");
        }
        if(StringUtils.isNotBlank(dateTo)){
            queryWrapper.apply("IFNULL(updated_at,created_at) < 1000 * UNIX_TIMESTAMP('" + DateUtil.formatUtcYYYYMMDDHHmmss(dateTo) + "')");
        }
        queryWrapper.orderByAsc("created_at").orderByAsc("id");
        return opLocationMapper.selectCount(queryWrapper);
    }

    /**
     * 分页获取DB场站数据
     * @param dateFrom
     * @param dateTo
     * @param offset
     * @param limit
     * @param operatorIdList
     * @return
     */
    private List<OpLocationEntity> getPageStation(String dateFrom, String dateTo, Integer offset, Integer limit,
                                                  List<String> operatorIdList, List<String> excludeOperatorIdList){
        QueryWrapper<OpLocationEntity> queryWrapper=new QueryWrapper<>();
        queryWrapper.eq("status", 1);
        queryWrapper.eq("deleted", 0);
        queryWrapper.eq("hubject_check", 1);
        if(!CollectionUtils.isEmpty(operatorIdList)){
            queryWrapper.in("operator_id", operatorIdList);
        }
        if(!CollectionUtils.isEmpty(excludeOperatorIdList)){
            queryWrapper.notIn("operator_id", excludeOperatorIdList);
        }

        if(StringUtils.isNotBlank(dateFrom)){
            queryWrapper.apply("IFNULL(updated_at,created_at) >= 1000 * UNIX_TIMESTAMP('" + DateUtil.formatUtcYYYYMMDDHHmmss(dateFrom) + "')");
        }
        if(StringUtils.isNotBlank(dateTo)){
            queryWrapper.apply("IFNULL(updated_at,created_at) < 1000 * UNIX_TIMESTAMP('" + DateUtil.formatUtcYYYYMMDDHHmmss(dateTo) + "')");
        }
        queryWrapper.orderByAsc("created_at").orderByAsc("id");
        queryWrapper.last("limit " + offset +","+ limit);
        return opLocationMapper.selectList(queryWrapper);
    }

    /**
     * 对象转化
     * @param location
     * @return
     */
    private OcpiLocationMappingVO convertOcpiLocationMappingVO(OpLocationEntity location){
        return OcpiLocationMappingVO.builder()
                .id(location.getId().toString())
                .operatorId(location.getOperatorId())
                .createTime(DateUtil.timeStamp2ISO8601(location.getCreatedAt()))
                .latitude(String.format("%.6f",Double.parseDouble(location.getLatitude())))
                .longitude(String.format("%.6f",Double.parseDouble(location.getLongitude())))
                .postCode(location.getPostalCode())
                .name(location.getName())
                .street(location.getAddress())
                .city(StringUtils.defaultIfBlank(location.getCity(), locationCity))
                .country(StringUtils.defaultIfBlank(CountryCodeCacheRunner.countryCodeMap.get(location.getCountry()), locationCountry))
                .timeZone(location.getZoneId())
                .updateTime(DateUtil.timeStamp2ISO8601(location.getUpdatedAt()))
                .build();
    }

    /**
     * LocationVO 值获取和设置（ocpi2.2）
     * @param locationMappingVO
     * @return
     */
    private OcpiV2LocationVO getLocationVO(OcpiLocationMappingVO locationMappingVO, String evseUid){
        String address = locationMappingVO.getStreet();
        if(StringUtils.isNotBlank(address) && address.length() > 45){
            address = address.substring(0,35) + "...";
        }
        OcpiV2LocationVO locationVO =  OcpiV2LocationVO.builder().id(locationMappingVO.getId())
                .country_code("FR")
                .party_id("AUT")
                .publish(true)
                .address(address)
                .name(locationMappingVO.getName())
                .postal_code(locationMappingVO.getPostCode())
                .last_updated(locationMappingVO.getUpdateTime())
//                .type(locationType)
                .city(locationMappingVO.getCity())
                .country(locationMappingVO.getCountry())
                .time_zone(locationMappingVO.getTimeZone())
                .build();

        GeoLocation coordinates = new GeoLocation();
        coordinates.setLatitude(String.format("%.6f",Double.parseDouble(locationMappingVO.getLatitude())));
        coordinates.setLongitude(String.format("%.6f",Double.parseDouble(locationMappingVO.getLongitude())));
        locationVO.setCoordinates(coordinates);

        String cpoId = "";
        try{
            RoamingRulesVO roamingRulesVO = billFeignClient.getIndependentCpoIdBySeller(locationMappingVO.getOperatorId()).getData();
            if(roamingRulesVO != null){
                cpoId = roamingRulesVO.getOcpiId();
            }
        }catch (Exception e){
            log.error("获取ocpiId异常");
        }

        if(StringUtils.isBlank(cpoId)){
            cpoId = countryCode + "*" + partyId;
        }

        List<OcpiV2EvseVO> evseVOList = new ArrayList<>();
        List<OpLocationEvseEntity> evseList;

        // 场站列表获取的数据是实时数据，cdr、session获取场站数据可能是历史数据，所以桩、计费可能是没有互联互通的也需要查
        boolean isRealTimeData = StringUtils.isBlank(evseUid);
        if(isRealTimeData){
            evseList = opLocationEvseMapper.getEvseListByLocationId(Long.valueOf(locationMappingVO.getId()),evseUid);
        }else{
            evseList = opLocationEvseMapper.getEvseListByEvseId(evseUid);
        }
        log.info("evseList :{}", JSON.toJSONString(evseList));
        if(!CollectionUtils.isEmpty(evseList)){
            String finalCpoId = cpoId;
            evseList.forEach(g -> {
                OcpiV2EvseVO ocpiV2EvseVO = getEvseVO(g, finalCpoId);
                if(ocpiV2EvseVO != null && !CollectionUtils.isEmpty(ocpiV2EvseVO.getConnectors())){
                    evseVOList.add(ocpiV2EvseVO);
                }
            });
        }
        locationVO.setEvses(evseVOList);

       /* QueryWrapper<OpLocationOperationEntity> queryWrapper=new QueryWrapper<>();
        queryWrapper.eq("status", 1);
        queryWrapper.eq("deleted", 0);
        queryWrapper.eq("location_id", locationMappingVO.getId());
        queryWrapper.select("operation_date","updated_at");*/
        //OpLocationOperationEntity opLocationOperationEntity = opLocationOperationMapper.selectOne(queryWrapper);
        HoursVO hoursVO = new HoursVO();
        hoursVO.setTwentyfourseven(true);
//        locationVO.setOpening_times(hoursVO);
        return locationVO;
    }


    /**
     * evseVO2数据获取（ocpi2.2）
     * @param evse
     * @return
     */
    private OcpiV2EvseVO getEvseVO(OpLocationEvseEntity evse, String cpoId) {
        log.info("getEvseVO evse:{}", JSON.toJSONString(evse));
        OcpiV2EvseVO evseVO = OcpiV2EvseVO.builder().uid(evse.getId().toString()).build();
        evseVO.setStatus("UNKNOWN");
        evseVO.setCapabilities(evseCapabilities);

        String pileSn = evse.getEvseSn().substring(0, evse.getEvseSn().indexOf("_"));
        String gunNo = evse.getEvseSn().substring(evse.getEvseSn().indexOf("_") + 1, evse.getEvseSn().length());
        evseVO.setEvse_id(cpoId + "*E" + pileSn + "*" + gunNo);
        try{
            String evseStatus = monitorFeignClient.queryStatusByEvseSn(evse.getEvseSn()).getData();
            log.info("枪{}:监控状态：{}", evse.getEvseSn(), evseStatus);
            if(StringUtils.isNotBlank(evseStatus)){
                switch (evseStatus){
                    case "Available":
                    case "Preparing":
                        evseVO.setStatus("AVAILABLE");
                        break;
                    case "Charging":
                        evseVO.setStatus("CHARGING");
                        break;
                    case "Unavailable":
                        evseVO.setStatus("INOPERATIVE");
                        break;
                    default:
                        evseVO.setStatus("BLOCKED");
                        break;
                }
            }
        }catch (Exception e){
            log.error("获取枪状态据异常： {}", e.getStackTrace());
        }

        List<OcpiV2ConnectorVO> connectors = ocpiV2ConnectorList(evse.getId());
        evseVO.setConnectors(connectors);
        evseVO.setLast_updated(DateUtil.timeStamp2ISO8601(evse.getUpdatedAt()));
        log.info("evseVO :{}", JSON.toJSONString(evseVO));
        return evseVO;
    }

    /**
     * 基于枪来获取connector数据（ocpi2.2）
     * @param evseId
     * @return
     */
    private List<OcpiV2ConnectorVO> ocpiV2ConnectorList(Long evseId){
        List<OcpiV2ConnectorVO> connectors = new ArrayList<>();
        try{
            String tariffId = null;
            try{
                TariffVO tariffVO = ocpiTariffFeignClient.getEvseTariff(evseId.toString()).getData();
                if(tariffVO != null){
                    tariffId = tariffVO.getId();
                }
            }catch (Exception e){
            }

            List<OpLocationConnectorEntity> connectorList = opLocationConnectorMapper.getConnectorListByEvseId(evseId);
            if(!CollectionUtils.isEmpty(connectorList)){
                String finalTariffId = tariffId;
                connectorList.forEach(connector ->{
                    String powerType = connector.getPowerType();
                    if(StringUtils.isNotBlank(powerType) && powerType.startsWith("DC")){
                        powerType = "DC";
                        connector.setFormat(StringUtils.defaultIfBlank(connector.getFormat(),"CABLE").toUpperCase());
                    }

                    int voltage = connector.getVoltage().intValue();
                    if(connector.getPower() <= 22 && connector.getVoltage().intValue() > 230){
                        voltage = 230;
                    }

                    OcpiV2ConnectorVO connectorVO = OcpiV2ConnectorVO.builder()
                            .id(connector.getId().toString())
                            .max_amperage(connector.getAmperage().intValue())
                            .max_voltage(voltage)
                            .standard(convertStandard(connector.getGunType()))
                            .power_type(StringUtils.defaultIfBlank(powerType, "AC_1_PHASE").toUpperCase())
                            .format(StringUtils.defaultIfBlank(connector.getFormat(),"SOCKET").toUpperCase())
                            .last_updated(DateUtil.timeStamp2ISO8601(connector.getUpdatedAt()))
                            .build();

                    connectorVO.setTariff_ids(Lists.newArrayList(finalTariffId));
                    connectors.add(connectorVO);
                });
            }
        }catch (Exception e){
            log.error("获取桩数据异常： {}", e.getStackTrace());
        }
        return connectors;
    }

    private String convertStandard(Integer gunType) {
        String result = "IEC_62196_T2";
        switch (gunType) {
            case 1:
                result = "IEC_62196_T2_COMBO";
                break;
            case 2:
                result = "CHADEMO";
                break;
            case 3:
                result = "IEC_62196_T2";
                break;
            case 4:
                result = "IEC_62196_T3C";
                break;
            case 5:
                result = "IEC_62196_T1";
                break;
            case 6:
                result = "DOMESTIC_E";
                break;
            case 7:
                result = "IEC_62196_T1_COMBO";
                break;
            case 10:
                result = "TESLA_R";
                break;
            default:
                break;
        }
        return result;
    }


}