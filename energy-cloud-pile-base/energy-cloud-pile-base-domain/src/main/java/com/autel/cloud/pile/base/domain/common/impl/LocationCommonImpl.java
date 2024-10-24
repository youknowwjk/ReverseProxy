package com.autel.cloud.pile.base.domain.common.impl;


import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.common.LocationCommon;
import com.autel.cloud.pile.base.domain.service.DistributeCostRuleService;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import lombok.extern.slf4j.Slf4j;
import org.elasticsearch.common.unit.DistanceUnit;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.GeoDistanceQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.query.FetchSourceFilterBuilder;
import org.springframework.data.elasticsearch.core.query.IndexBoost;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.stereotype.Component;

import javax.validation.constraints.NotNull;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @Author MingLong A22599
 * @Date 2022/11/25
 * @Function 处理场站信息的公共服务 实现类
 */
@Slf4j
@Component
public class LocationCommonImpl implements LocationCommon {

    @Autowired
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

    @Autowired
    private DistributeCostRuleService distributeCostRuleService;

    // 距离长度
    private static final Integer DISTANCE = 1;

    // 精度
    private static final BigDecimal PRECISION = new BigDecimal("0.00001");

    // 随机纬度下限值
    private static final BigDecimal RANDOM_LATITUDE_LOW = new BigDecimal("-90");

    // 随机纬度上限值
    private static final BigDecimal RANDOM_LATITUDE_HIGH = new BigDecimal("90");

    // 随机经度下限值
    private static final BigDecimal RANDOM_LONGITUDE_LOW = new BigDecimal("-180");

    // 随机经度上限值
    private static final BigDecimal RANDOM_LONGITUDE_HIGH = new BigDecimal("180");

    /**
     * @param latitude   纬度值 不能为空
     * @param longitude  经度值 不能为空
     * @param locationId 场站id 新增时无场站id, 修改时才有场站id
     * @return 设置的结果
     * @function 设置纬度和经度
     */
    @Override
    public Map<String, String> setTheLatitudeAndLongitudeOfTheStation(@NotNull String latitude, @NotNull String longitude, Long locationId) {

        log.info("=====>>>>> LocationCommonImpl.setTheLatitudeAndLongitudeOfTheStation latitude : {} and longitude : {} and locationId : {}", JSON.toJSONString(latitude), JSON.toJSONString(longitude), JSON.toJSONString(locationId));

        // 构造返回结果
        Map<String, String> latitudeLongitudeMap = new HashMap<>();

        // 存入站点的纬度键值对
        latitudeLongitudeMap.put("latitude", latitude);
        // 存入站点的经度键值对
        latitudeLongitudeMap.put("longitude", longitude);

        // 构建ES查询条件
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();

        // todo 按经纬度查询一公里范围内的站点信息
        GeoDistanceQueryBuilder geoDistanceQueryBuilder = new GeoDistanceQueryBuilder("location");

        //中心点的构建
        geoDistanceQueryBuilder.point(Double.valueOf(latitude), Double.valueOf(longitude));

        //范围设定
        geoDistanceQueryBuilder.distance(DISTANCE, DistanceUnit.KILOMETERS);

        queryBuilder.must(geoDistanceQueryBuilder);

        if (locationId != null) {
            // 修改时需要排除自己
            queryBuilder.mustNot(QueryBuilders.termQuery("id", locationId));
        }

        // 拼接查询条件
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withSourceFilter(new FetchSourceFilterBuilder().withIncludes("id", "latitude", "longitude").build())
//                .withFields("id", "latitude", "longitude") 不要使用这个
                .build();

        log.info("=====>>>>> LocationCommonImpl.setTheLatitudeAndLongitudeOfTheStation searchQuery : {}", JSON.toJSONString(searchQuery));

        // 获得查询结果集合
        List<OpLocationElasticDTO> opLocationEvseExpandElasticDTOList =
//                elasticsearchRestTemplate.queryForList(searchQuery, OpLocationElasticDTO.class);
        elasticsearchRestTemplate.search(searchQuery, OpLocationElasticDTO.class)
                .stream().map(SearchHit::getContent).collect(Collectors.toList());

        log.info("=====>>>>> LocationCommonImpl.setTheLatitudeAndLongitudeOfTheStation opLocationEvseExpandElasticDTOList : {}", JSON.toJSONString(opLocationEvseExpandElasticDTOList));

        // 原始坐标是否被占用的标志 (一般不会被占用)
        Boolean flag = false;

        if (ObjectUtils.isNotEmpty(opLocationEvseExpandElasticDTOList)) {
            for (OpLocationElasticDTO opLocationElasticDTO : opLocationEvseExpandElasticDTOList) {
                if (latitude.equals(opLocationElasticDTO.getLatitude()) && longitude.equals(opLocationElasticDTO.getLongitude())) {
                    // 原始坐标已被占用，需要改变标志
                    flag = true;
                    break;
                }
            }
        }

        if (flag) {
            // 原始坐标已被占用，需要改变坐标
            // 获得随机坐标列表
            List<List<String>> randomCoordinateList = this.randomCoordinate(latitude, longitude);

            log.info("=====>>>>> LocationCommonImpl.setTheLatitudeAndLongitudeOfTheStation randomCoordinateList : {}", JSON.toJSONString(randomCoordinateList));

            for (List<String> stringList : randomCoordinateList) {

                // 随机纬度
                String randomLatitude = stringList.get(0);
                // 随机经度
                String randomLongitude = stringList.get(1);

                // 该坐标是否已被占用的标志，该随机坐标默认没有被占用
                Boolean isItOccupied = false;

                for (OpLocationElasticDTO opLocationElasticDTO : opLocationEvseExpandElasticDTOList) {

                    // 已被占用的纬度
                    String latitudeAlreadyExists = opLocationElasticDTO.getLatitude();
                    // 已被占用的经度
                    String longitudeAlreadyExists = opLocationElasticDTO.getLongitude();

                    if (randomLatitude.equals(latitudeAlreadyExists) && randomLongitude.equals(longitudeAlreadyExists)) {
                        // 这个随机坐标已被占用，需要改变标志
                        isItOccupied = true;
                        break;
                    }
                }

                if (!isItOccupied) {
                    // 该随机坐标没有被占用，需要改变返回结果
                    // 需要覆盖存入站点的纬度键值对
                    latitudeLongitudeMap.put("latitude", randomLatitude);
                    // 需要覆盖存入站点的经度键值对
                    latitudeLongitudeMap.put("longitude", randomLongitude);
                    break;
                }
            }

        }

        log.info("=====>>>>> LocationCommonImpl.setTheLatitudeAndLongitudeOfTheStation latitudeLongitudeMap : {}", JSON.toJSONString(latitudeLongitudeMap));

        return latitudeLongitudeMap;
    }

    /**
     * @param latitude
     * @param longitude
     * @return 返回随机坐标列表
     * @function 设置地址时，如果经纬度完全一样，经纬度小数点后第七位可以自动 + 1或 - 1直到经纬度唯一
     */
    private List<List<String>> randomCoordinate(@NotNull String latitude, @NotNull String longitude) {

        log.info("=====>>>>> LocationCommonImpl.randomCoordinate latitude : {} and longitude : {}", JSON.toJSONString(latitude), JSON.toJSONString(longitude));

        // 返回对象
        List<List<String>> randomCoordinateList = new ArrayList<>();

        // 需要转换原始坐标 String -> BigDecimal
        // 原始纬度
        BigDecimal originalLatitude = new BigDecimal(latitude);
        // 原始经度
        BigDecimal originalLongitude = new BigDecimal(longitude);

        // 随机纬度
        BigDecimal randomLatitude;
        // 随机经度
        BigDecimal randomLongitude;

        // randomCoordinateA (? - PRECISION, ? - PRECISION)
        List<String> randomCoordinateA = new ArrayList<>();
        randomLatitude = originalLatitude.subtract(PRECISION);
        randomLongitude = originalLongitude.subtract(PRECISION);
        if (randomLatitude.compareTo(RANDOM_LATITUDE_LOW) <0) {
            randomLatitude = RANDOM_LATITUDE_LOW;
        }
        if (randomLongitude.compareTo(RANDOM_LONGITUDE_LOW) <0) {
            randomLongitude = RANDOM_LONGITUDE_LOW;
        }
        randomCoordinateA.add(randomLatitude.toPlainString());
        randomCoordinateA.add(randomLongitude.toPlainString());
        randomCoordinateList.add(randomCoordinateA);

        // randomCoordinateB (? - PRECISION, ?)
        List<String> randomCoordinateB = new ArrayList<>();
        randomLatitude = originalLatitude.subtract(PRECISION);
        randomLongitude = originalLongitude;
        if (randomLatitude.compareTo(RANDOM_LATITUDE_LOW) <0) {
            randomLatitude = RANDOM_LATITUDE_LOW;
        }
        randomCoordinateB.add(randomLatitude.toPlainString());
        randomCoordinateB.add(randomLongitude.toPlainString());
        randomCoordinateList.add(randomCoordinateB);

        // randomCoordinateC (? - PRECISION, ? + PRECISION)
        List<String> randomCoordinateC = new ArrayList<>();
        randomLatitude = originalLatitude.subtract(PRECISION);
        randomLongitude = originalLongitude.add(PRECISION);
        if (randomLatitude.compareTo(RANDOM_LATITUDE_LOW)<0) {
            randomLatitude = RANDOM_LATITUDE_LOW;
        }
        if (randomLongitude.compareTo(RANDOM_LONGITUDE_HIGH)>0) {
            randomLongitude = RANDOM_LONGITUDE_HIGH;
        }
        randomCoordinateC.add(randomLatitude.toPlainString());
        randomCoordinateC.add(randomLongitude.toPlainString());
        randomCoordinateList.add(randomCoordinateC);

        // randomCoordinateD (?, ? - PRECISION)
        List<String> randomCoordinateD = new ArrayList<>();
        randomLatitude = originalLatitude;
        randomLongitude = originalLongitude.subtract(PRECISION);
        if (randomLongitude.compareTo(RANDOM_LONGITUDE_LOW)<0) {
            randomLongitude = RANDOM_LONGITUDE_LOW;
        }
        randomCoordinateD.add(randomLatitude.toPlainString());
        randomCoordinateD.add(randomLongitude.toPlainString());
        randomCoordinateList.add(randomCoordinateD);

        // randomCoordinateE (?, ? + PRECISION)
        List<String> randomCoordinateE = new ArrayList<>();
        randomLatitude = originalLatitude;
        randomLongitude = originalLongitude.add(PRECISION);
        if (randomLongitude.compareTo(RANDOM_LONGITUDE_HIGH) >0) {
            randomLongitude = RANDOM_LONGITUDE_HIGH;
        }
        randomCoordinateE.add(randomLatitude.toPlainString());
        randomCoordinateE.add(randomLongitude.toPlainString());
        randomCoordinateList.add(randomCoordinateE);

        // randomCoordinateF (? + PRECISION, ? - PRECISION)
        List<String> randomCoordinateF = new ArrayList<>();
        randomLatitude = originalLatitude.add(PRECISION);
        randomLongitude = originalLongitude.subtract(PRECISION);
        if (randomLatitude.compareTo(RANDOM_LATITUDE_HIGH)>0) {
            randomLatitude = RANDOM_LATITUDE_HIGH;
        }
        if (randomLongitude.compareTo(RANDOM_LONGITUDE_LOW) <0) {
            randomLongitude = RANDOM_LONGITUDE_LOW;
        }
        randomCoordinateF.add(randomLatitude.toPlainString());
        randomCoordinateF.add(randomLongitude.toPlainString());
        randomCoordinateList.add(randomCoordinateF);

        // randomCoordinateG (? + PRECISION, ?)
        List<String> randomCoordinateG = new ArrayList<>();
        randomLatitude = originalLatitude.add(PRECISION);
        randomLongitude = originalLongitude;
        if (randomLatitude.compareTo(RANDOM_LATITUDE_HIGH) >0) {
            randomLatitude = RANDOM_LATITUDE_HIGH;
        }
        randomCoordinateG.add(randomLatitude.toPlainString());
        randomCoordinateG.add(randomLongitude.toPlainString());
        randomCoordinateList.add(randomCoordinateG);

        // randomCoordinateH (? + PRECISION, ? + PRECISION)
        List<String> randomCoordinateH = new ArrayList<>();
        randomLatitude = originalLatitude.add(PRECISION);
        randomLongitude = originalLongitude.add(PRECISION);
        if (randomLatitude.compareTo(RANDOM_LATITUDE_HIGH)>0) {
            randomLatitude = RANDOM_LATITUDE_HIGH;
        }
        if (randomLongitude.compareTo(RANDOM_LONGITUDE_HIGH) >0) {
            randomLongitude = RANDOM_LONGITUDE_HIGH;
        }
        randomCoordinateH.add(randomLatitude.toPlainString());
        randomCoordinateH.add(randomLongitude.toPlainString());
        randomCoordinateList.add(randomCoordinateH);

        log.info("=====>>>>> LocationCommonImpl.randomCoordinate randomCoordinateList : {}", JSON.toJSONString(randomCoordinateList));

        return randomCoordinateList;
    }

    /**
     * 版本号比较
     *
     * @param userVersion
     * @param supportUpgradeVersion
     * @return 0代表相等，1代表左边大，-1代表右边大
     * Utils.compareVersion("1.0.358_20180820090554","1.0.358_20180820090553")=1
     */
    @Override
    public int compareVersion(String userVersion, String supportUpgradeVersion) {
        if (userVersion.equals(supportUpgradeVersion)) {
            return 0;
        }
        String[] version1Array = userVersion.split("[._]");
        String[] version2Array = supportUpgradeVersion.split("[._]");
        int index = 0;
        int minLen = Math.min(version1Array.length, version2Array.length);
        long diff = 0;

        while (index < minLen
                && (diff = Long.parseLong(version1Array[index])
                - Long.parseLong(version2Array[index])) == 0) {
            index++;
        }
        if (diff == 0) {
            for (int i = index; i < version1Array.length; i++) {
                if (Long.parseLong(version1Array[i]) > 0) {
                    return 1;
                }
            }

            for (int i = index; i < version2Array.length; i++) {
                if (Long.parseLong(version2Array[i]) > 0) {
                    return -1;
                }
            }
            return 0;
        } else {
            return diff > 0 ? 1 : -1;
        }
    }

    @Override
    public Result<Boolean> dispatchTariffOfPublicPileByTariffId(Long tariffId, Long userId, Boolean newRelease) {

        log.info("=====>>>>> LocationCommonImpl.dispatchTariffOfPublicPileByTariffId tariffId : {} and userId : {} and newRelease : {}", JSON.toJSONString(tariffId), JSON.toJSONString(userId), JSON.toJSONString(newRelease));

        return distributeCostRuleService.dispatchTariffOfPublicPileByTariffId(tariffId, userId, newRelease);
    }

    @Override
    public Boolean issueBillingRule(List<String> pileSnList) {

        log.info("=====>>>>> LocationCommonImpl.issueBillingRule pileSnList : {}",
                JSON.toJSONString(pileSnList));

        return distributeCostRuleService.issueBillingRule(pileSnList);
    }
}
