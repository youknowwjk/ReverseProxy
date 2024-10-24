package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.pile.base.domain.repository.OpLocationEvseRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileEvseRepository;
import com.autel.cloud.pile.base.domain.repository.RuleRepository;
import com.autel.cloud.pile.base.domain.service.RuleLocationPileService;
import com.autel.cloud.pile.base.dto.GunInformationDTO;
import com.autel.cloud.pile.base.dto.PileDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.RuleLocationPileMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.RuleEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.RuleLocationPileEntity;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.elasticsearch.index.query.QueryBuilders;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
* @author temp
* @description 针对表【tb_rule_location_pile(进场控制规则与桩关系表)】的数据库操作Service实现
* @createDate 2022-07-07 21:19:47
*/
@Service
@Slf4j
public class RuleLocationPileServiceImpl extends ServiceImpl<RuleLocationPileMapper, RuleLocationPileEntity>
    implements RuleLocationPileService {
    @Resource
    private OpLocationPileEvseRepository opLocationPileEvseService;
    @Resource
    private OpLocationEvseRepository opLocationEvseRepository;

    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

    @Resource
    private RuleRepository ruleRepository;

    @Override
    public boolean batchInsert(Long ruleId, List<PileDTO> piles) {
        List<RuleLocationPileEntity> insertList = new ArrayList<>();
        if (CollectionUtils.isEmpty(piles)) {
            return true;
        }
        piles.forEach(p->{
            RuleLocationPileEntity entity = new RuleLocationPileEntity();
            entity.setRuleId(ruleId);
            entity.setLocationId(p.getLocationId());
            entity.setPileId(p.getPileId());
            entity.setCreateTime(System.currentTimeMillis());
            insertList.add(entity);
        });
        return this.saveBatch(insertList);
    }

    @Override
    public boolean batchInsertEvse(Long ruleId, List<GunInformationDTO> piles) {
        List<RuleLocationPileEntity> insertList = new ArrayList<>();
        if (CollectionUtils.isEmpty(piles)) {
            return true;
        }
        piles.forEach(p->{
            RuleLocationPileEntity entity = new RuleLocationPileEntity();
            entity.setRuleId(ruleId);
            entity.setLocationId(p.getLocationId());
            entity.setPileId(p.getPileId());
            entity.setCreateTime(System.currentTimeMillis());
            entity.setPileSn(p.getSn());
            entity.setEvseSn(p.getSn() + "_" + p.getGunNumber());
            entity.setPileName(p.getPileName());
            insertList.add(entity);
        });
        if (!insertList.isEmpty()) {
            List<String> tmpList = insertList.stream().map(RuleLocationPileEntity::getEvseSn).collect(Collectors.toList());
            LambdaQueryWrapper<RuleLocationPileEntity> query = Wrappers.lambdaQuery();
            query.in(RuleLocationPileEntity::getEvseSn, tmpList);
            List<RuleLocationPileEntity> existList = this.list(query);
            if (!CollectionUtils.isEmpty(existList)) {
                this.removeByIds(existList.stream().map(RuleLocationPileEntity::getId).collect(Collectors.toList()));
            }
        }
        return this.saveBatch(insertList);
    }
    @Override
    public boolean deleteRuleRelateByLocationId(Long locationId) {
        log.info("deleteRuleRelateByLocationId,locationId={}",locationId);
        int delete = this.getBaseMapper().delete(new LambdaQueryWrapper<RuleLocationPileEntity>()
                .eq(RuleLocationPileEntity::getLocationId, locationId));
        log.info("deleteRuleRelateByLocationId,delete={}",delete);
        return true;
    }

    @Override
    public boolean deleteRuleRelateByPileId(Long pileId) {
        log.info("deleteRuleRelateByPileId,pileId={}",pileId);
        int delete = this.getBaseMapper().delete(new LambdaQueryWrapper<RuleLocationPileEntity>()
                .eq(RuleLocationPileEntity::getPileId, pileId));
        log.info("deleteRuleRelateByPileId,delete={}",delete);
        return true;
    }

    @Override
    public List<RuleLocationPileEntity> findAll() {
        return this.list(new LambdaQueryWrapper<RuleLocationPileEntity>());
    }

    @Override
    public Integer syncRuleToEvse() {
        Integer result = 0;
        int page = 1;
        int pageSize = 100;

        LambdaQueryWrapper<RuleLocationPileEntity> query = Wrappers.lambdaQuery();
        query.isNotNull(RuleLocationPileEntity::getEvseSn);
        query.orderByDesc(RuleLocationPileEntity::getId);
        List<RuleLocationPileEntity> entityList = new ArrayList<>();
        List<String> evseSnList = new ArrayList<>();
        while (true) {
            List<RuleLocationPileEntity> tmpList = this.page(new Page<>(page, pageSize), query).getRecords();
            if (CollectionUtils.isEmpty(tmpList)) {
                break;
            }
            entityList.addAll(tmpList);
            List<String> tmp = tmpList.stream().map(RuleLocationPileEntity::getEvseSn).distinct().collect(Collectors.toList());
            evseSnList.addAll(tmp);
            page++;
        }
        if (CollectionUtils.isEmpty(entityList)) {
            log.info("syncRuleToEvse,entityList is empty.");
            return result;
        }
        Map<String, RuleLocationPileEntity> entityMap = entityList.stream().collect(Collectors.toMap(RuleLocationPileEntity::getEvseSn, e -> e, (f, s) -> f));
        List<Long> ids = entityList.stream().map(RuleLocationPileEntity::getRuleId).distinct().collect(Collectors.toList());
        List<RuleEntity> listToUse = this.ruleRepository.listByIds(ids).stream().collect(Collectors.toList());
        Map<Long, RuleEntity> ruleEntityMap = listToUse.stream().collect(Collectors.toMap(RuleEntity::getId, e -> e, (f, s) -> f));
        List<OpLocationEvseElasticDTO> evseDtoList = this.elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(QueryBuilders.termsQuery("evseSn", evseSnList)).build(), OpLocationEvseElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
        List<OpLocationEvseElasticDTO> updateDtoList = new ArrayList<>();
        evseDtoList.stream().forEach(evseDto -> {
            String evseSn = evseDto.getEvseSn();
            RuleLocationPileEntity entity = entityMap.get(evseSn);
            if (entity == null) {
                return;
            }
            evseDto.setRuleId(entity.getRuleId());
            RuleEntity ruleEntity = ruleEntityMap.get(entity.getRuleId());
            if (ruleEntity == null) {
                return;
            }
            evseDto.setRuleName(ruleEntity.getName());
            updateDtoList.add(evseDto);
        });
        if (CollectionUtils.isEmpty(updateDtoList)) {
            log.info("syncRuleToEvse,updateDtoList is empty.");
            return result;
        }
        this.opLocationEvseRepository.savBatchEs(updateDtoList);
        result = updateDtoList.size();
        return result;
    }

    @Override
    public List<String> selectBindEvse(List<String> snList) {
        List<RuleLocationPileEntity> ruleLocationPileEntities = this.baseMapper.selectList(new LambdaQueryWrapper<RuleLocationPileEntity>().in(RuleLocationPileEntity::getEvseSn, snList));
       if (CollectionUtils.isEmpty(ruleLocationPileEntities)) {
           return new ArrayList<>();
       }
       return ruleLocationPileEntities.stream().map(RuleLocationPileEntity::getEvseSn).collect(Collectors.toList());
    }
}




