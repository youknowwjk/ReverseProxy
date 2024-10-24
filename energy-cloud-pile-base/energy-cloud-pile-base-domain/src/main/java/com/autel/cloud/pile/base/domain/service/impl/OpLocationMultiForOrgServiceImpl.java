package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.infrastructure.feign.PileUserServiceFeign;
import com.autel.cloud.pile.base.vo.OrganizationVO;
import com.google.common.collect.Lists;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;

@Service
@Slf4j
@Data
public class OpLocationMultiForOrgServiceImpl implements Callable<Map<Long, OrganizationVO>> {

    private Set<Long> orgIdSet;

    @Autowired
    private PileUserServiceFeign pileUserServiceFeign;

    private Map<Long, OrganizationVO> buildOrgIdOrgEntityMap(Set<Long> orgIdSet) {
        Map<Long, OrganizationVO> orgIdOrgEntityMap = new HashMap<>();
        log.info("站点机构id集合：{}", JSON.toJSONString(orgIdSet));
        if (CollectionUtils.isNotEmpty(orgIdSet)) {
            try {
                Result<List<OrganizationVO>> orgResult = pileUserServiceFeign.getOrgListByIds(Lists.newArrayList(orgIdSet));
                log.info("查询组织机构id{}  的实体数据{}：", JSON.toJSONString(orgIdSet), JSON.toJSONString(orgResult));
                if (orgResult != null && orgResult.getCode() == HttpStatus.OK.value()) {
                    List<OrganizationVO> organizationVOList = orgResult.getData();
                    if (CollectionUtils.isNotEmpty(organizationVOList)) {
                        organizationVOList.forEach(organizationVO -> orgIdOrgEntityMap.put(organizationVO.getId(), organizationVO));
                    }
                }
            } catch (Exception e) {
                log.error("OpLocationMultiForOrgServiceImpl-buildOrgIdOrgEntityMap:" + e);
            }
        }
        return orgIdOrgEntityMap;
    }

    @Override
    public Map<Long, OrganizationVO> call() {
        return buildOrgIdOrgEntityMap(orgIdSet);
    }
}
