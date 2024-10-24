package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.collection.CollUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.service.OpLocationPileEvseService;
import com.autel.cloud.pile.base.domain.service.SellerService;
import com.autel.cloud.pile.base.dto.OpLocationListDTO;
import com.autel.cloud.pile.base.dto.SellerInfoDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.SaasAccessFeignClient;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationMapper;
import com.autel.cloud.pile.base.vo.SellerVO;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.vo.SellerDetailVO;
import com.google.common.collect.Lists;
import lombok.extern.log4j.Log4j2;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.vo.SellerDetailVO;
import com.google.common.collect.Lists;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.query.FetchSourceFilter;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.core.env.Environment;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * Author:   A19011
 * Description: SellerService
 * Date:     2022/5/12 10:39
 *
 * @Version 0.0.1-SNAPSHOT
 */
@Service
@Log4j2
public class SellerServiceImpl implements SellerService {
    @Autowired
    OpLocationMapper opLocationMapper;
    @Autowired
    SaasAccessFeignClient saasAccessFeignClient;
    @Autowired
    private OpLocationPileEvseService opLocationPileEvseService;
    @Autowired
    private StringRedisTemplate stringRedisTemplate;
    @Autowired
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

    @Resource
    private PileUserFeign pileUserFeign;

    @Autowired
    private Environment environment;


    @Override
    public String getSellerInfoBySn(String evseSn) {
        String operatorId = this.getSellerIdInfoBySn(evseSn);
        SellerVO sellerVO = saasAccessFeignClient.querySellerById(Long.parseLong(operatorId)).getData();
        return sellerVO.getNickname();
    }

    @Override
    public String getSellerIdInfoBySn(String evseSn) {
        String pileSn = evseSn.split("_")[0];
        return this.getSellerIdByPileSn(pileSn);
    }

    @Override
    public String getSellerIdByPileSn(String pileSn) {
        String snOperatorIdKey = RedisKeyConstant.getStringSnOperatorIdKey(pileSn);
        String operatorId = this.stringRedisTemplate.opsForValue().get(snOperatorIdKey);
        if (StringUtils.hasText(operatorId)) {
            return operatorId;
        }
        BoolQueryBuilder query = QueryBuilders.boolQuery();
        query.must(QueryBuilders.termQuery("pileSn", pileSn));

        //如果结果中有多条只取一条
        NativeSearchQuery build = new NativeSearchQueryBuilder()
                .withQuery(query)
                .withPageable(PageRequest.of(0, 1))
                .withSourceFilter(new FetchSourceFilter(new String[]{"operatorId"}, null))
                .build();
        List<OpLocationPileEvseElasticDTO> pileDtoList = this.elasticsearchRestTemplate.search(build, OpLocationPileEvseElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(pileDtoList)) {
            operatorId = pileDtoList.get(0).getOperatorId().toString();
            if (StringUtils.hasText(operatorId)) {
                //设置缓存
                this.stringRedisTemplate.opsForValue().set(snOperatorIdKey, operatorId, 30L, TimeUnit.DAYS);
            }
            return operatorId;
        }
        return null;
    }

    @Override
    public List<String> getPileSnBySellId(String sellerId) {
        Long id = Long.valueOf(sellerId);
        List<String> include = new ArrayList<>();
        include.add("id");
        include.add("pileSn");
        List<OpLocationPileEvseElasticDTO> pileDtoList = opLocationPileEvseService.findList(id, include);
        if (!CollectionUtils.isEmpty(pileDtoList)) {
            List<String> result = pileDtoList.stream().map(OpLocationPileEvseElasticDTO::getPileSn).collect(Collectors.toList());
            return result;
        }
        return null;
    }

    @Override
    public SellerInfoDTO getSellerInfoByPileSn(String pileSn) {
        String operatorId = this.getSellerIdByPileSn(pileSn);
        SellerInfoDTO sellerInfoDTO = new SellerInfoDTO();
        String env = environment.getProperty("NS_ENV", "N/A");
        log.info("getSellerInfoByPileSn.env: {}", JSON.toJSONString(env));
        sellerInfoDTO.setEnv(env);

        if (org.apache.commons.lang3.StringUtils.isNotBlank(operatorId)) {
            List<OpLocationListDTO> locationByPileSnList = opLocationMapper.getLocationByPileSnList(Lists.newArrayList(pileSn));
            if (CollUtil.isNotEmpty(locationByPileSnList)) {
                OpLocationListDTO opLocationListDTO = locationByPileSnList.get(0);
                sellerInfoDTO.setLocationId(String.valueOf(opLocationListDTO.getId()));
                sellerInfoDTO.setLocationName(opLocationListDTO.getLocationName());
            }
            sellerInfoDTO.setOperatorId(operatorId);
            Result<SellerDetailVO> sellerDetailVOResult = pileUserFeign.detail(Long.valueOf(operatorId));
            if (!ObjectUtils.isEmpty(sellerDetailVOResult) && !ObjectUtils.isEmpty(sellerDetailVOResult.getData())) {
                SellerDetailVO sellerDetailVO = sellerDetailVOResult.getData();
                if (org.apache.commons.lang3.StringUtils.isNotBlank(sellerDetailVO.getName())) {
                    sellerInfoDTO.setOperatorName(sellerDetailVO.getName());
                }
            }
        }
        return sellerInfoDTO;
    }

}
