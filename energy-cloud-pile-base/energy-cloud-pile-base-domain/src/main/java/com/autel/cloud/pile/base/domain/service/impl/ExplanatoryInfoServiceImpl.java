package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.collection.CollUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;
import com.autel.cloud.authoritymanager.dto.BaseMenuDto;
import com.autel.cloud.authoritymanager.feign.AuthorityManagerFeign;
import com.autel.cloud.base.common.enums.ResultCodeEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.http.vo.PageReqVO;
import com.autel.cloud.pile.base.domain.convert.ExplanatoryInfoConverter;
import com.autel.cloud.pile.base.domain.service.ExplanatoryInfoService;
import com.autel.cloud.pile.base.dto.ExplanatoryInfoDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.ExplanatoryInfoMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ExplanatoryInfoEntity;
import com.autel.cloud.pile.base.vo.ExplanatoryInfoVO;
import com.autel.cloud.pile.base.vo.app.PageDTO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Service
@Slf4j
public class ExplanatoryInfoServiceImpl extends ServiceImpl<ExplanatoryInfoMapper, ExplanatoryInfoEntity> implements ExplanatoryInfoService {
    @Resource
    AuthorityManagerFeign authorityManagerFeign;

    @Resource
    StringRedisTemplate stringRedisTemplate;

    public static final String PILE_BASE_EXPLANATORY_MENU_MAP_CACHE = "PILE_BASE_EXPLANATORY_MENU_MAP_CACHE";

    @Override
    public Result<ExplanatoryInfoVO> saveOrUpdate(ExplanatoryInfoDTO explanatoryInfoDTO) {
        final ExplanatoryInfoEntity explanatoryInfoEntity = ExplanatoryInfoConverter.explanatoryInfoConverter.dto2Entity(explanatoryInfoDTO);
        log.info("explanatoryEntity: {}", JSON.toJSONString(explanatoryInfoEntity));
        if (Objects.isNull(explanatoryInfoEntity.getId())) {
            //保存
            final boolean save = save(explanatoryInfoEntity);
            log.info("save: {}", save);
            if (save) {
                return Result.ofSucceed(ExplanatoryInfoConverter.explanatoryInfoConverter.entity2VO(explanatoryInfoEntity));
            }
            return Result.ofFailed(ResultCodeEnum.INTERNAL_SERVER_ERROR);
        }
        //更新
        final boolean update = updateById(explanatoryInfoEntity);
        log.info("update:{}", update);
        if (update) {
            return Result.ofSucceed(ExplanatoryInfoConverter.explanatoryInfoConverter.entity2VO(explanatoryInfoEntity));
        }
        return Result.ofFailed(ResultCodeEnum.INTERNAL_SERVER_ERROR);
    }

    @Override
    public Result<PageDTO<ExplanatoryInfoVO>> page(PageReqVO<ExplanatoryInfoDTO> explanatoryInfoSearchDTO) {
        log.info("explanatoryInfoSearchDTO:{}", JSON.toJSONString(explanatoryInfoSearchDTO));
        //查询Menu信息；
        HashMap<Long, BaseMenuDto> menuMap = getLongBaseMenuDtoHashMap(explanatoryInfoSearchDTO.getData().getLanguage());
        log.info("menuMap: \n{}", JSON.toJSONString(menuMap));
        //查询参数
        final ExplanatoryInfoDTO searchDTOData = explanatoryInfoSearchDTO.getData();
        LambdaQueryWrapper<ExplanatoryInfoEntity> queryWrapper = new LambdaQueryWrapper<>();
        Optional.ofNullable(searchDTOData.getId()).ifPresent(id -> queryWrapper.eq(ExplanatoryInfoEntity::getId,id));
        Optional.ofNullable(searchDTOData.getMenuId()).ifPresent(menuId -> queryWrapper.eq(ExplanatoryInfoEntity::getMenuId,menuId));
        Optional.ofNullable(searchDTOData.getLabelKey()).ifPresent(labelKey -> queryWrapper.eq(ExplanatoryInfoEntity::getLabelKey,labelKey));
        Optional.ofNullable(searchDTOData.getLanguage()).ifPresent(language -> queryWrapper.eq(ExplanatoryInfoEntity::getLanguage,language));
        //page参数
        final Integer page = explanatoryInfoSearchDTO.getPage();
        final Integer pageSize = explanatoryInfoSearchDTO.getPageSize();
        Page<ExplanatoryInfoEntity> pageCondition = new Page<>();
        pageCondition.setCurrent(page);
        pageCondition.setSize(pageSize);
        queryWrapper.orderByDesc(ExplanatoryInfoEntity::getCreateTime).orderByDesc(ExplanatoryInfoEntity::getId);
        //查询
        final Page<ExplanatoryInfoEntity> pageRes = page(pageCondition, queryWrapper);
        final List<ExplanatoryInfoEntity> records = pageRes.getRecords();
        //数据处理
        final List<ExplanatoryInfoVO> collect = new ArrayList<>();
        for (ExplanatoryInfoEntity explanatoryInfoEntity : records) {
            ExplanatoryInfoVO explanatoryInfoVO = ExplanatoryInfoConverter.explanatoryInfoConverter.entity2VO(explanatoryInfoEntity);
            final BaseMenuDto baseMenuDto = menuMap.get(explanatoryInfoEntity.getMenuId());
            if (Objects.isNull(baseMenuDto)) {
                continue;
            }
            explanatoryInfoVO.setMenuName(baseMenuDto.getMenuName());
            explanatoryInfoVO.setWebMenuPath(baseMenuDto.getWebMenuPath());
            collect.add(explanatoryInfoVO);
        }
        PageDTO<ExplanatoryInfoVO> result = new PageDTO<>();
        result.setPageNo(page);
        result.setTotalCount(pageRes.getTotal());
        result.setPages(pageRes.getPages());
        result.setPageSize(pageSize);
        result.setData(collect);
        return Result.ofSucceed(result);
    }

    @Override
    public Result<PageDTO<ExplanatoryInfoVO>> searchConfigPages(PageReqVO<ExplanatoryInfoDTO> explanatoryInfoSearchDTO, String language) {
        LambdaQueryWrapper<ExplanatoryInfoEntity> queryWrapper = new LambdaQueryWrapper<>();
        Optional.ofNullable(language).ifPresent(lan -> queryWrapper.eq(ExplanatoryInfoEntity::getLanguage,lan));
        queryWrapper.groupBy(ExplanatoryInfoEntity::getMenuId);
        queryWrapper.orderByDesc(ExplanatoryInfoEntity::getCreateTime).orderByDesc(ExplanatoryInfoEntity::getId);
        //page参数
        final Integer page = explanatoryInfoSearchDTO.getPage();
        final Integer pageSize = explanatoryInfoSearchDTO.getPageSize();
        Page<ExplanatoryInfoEntity> pageCondition = new Page<>();
        pageCondition.setCurrent(page);
        pageCondition.setSize(pageSize);
        //查询
        final Page<ExplanatoryInfoEntity> pageRes = page(pageCondition, queryWrapper);
        final List<ExplanatoryInfoEntity> records = pageRes.getRecords();
        final HashMap<Long, BaseMenuDto> menuMap = getLongBaseMenuDtoHashMap(language);
        final List<ExplanatoryInfoVO> collect = records.stream().map(entity -> {
            ExplanatoryInfoVO explanatoryInfoVO = new ExplanatoryInfoVO();
            final BaseMenuDto baseMenuDto = menuMap.get(entity.getMenuId());
            explanatoryInfoVO.setMenuId(entity.getMenuId().toString());
            explanatoryInfoVO.setMenuName(baseMenuDto.getMenuName());
            explanatoryInfoVO.setWebMenuPath(baseMenuDto.getWebMenuPath());
            return explanatoryInfoVO;
        }).collect(Collectors.toList());

        PageDTO<ExplanatoryInfoVO> result = new PageDTO<>();
        result.setPageNo(page);
        result.setTotalCount(pageRes.getTotal());
        result.setPages(pageRes.getPages());
        result.setPageSize(pageSize);
        result.setData(collect);
        return Result.ofSucceed(result);
    }

    private HashMap<Long, BaseMenuDto> getLongBaseMenuDtoHashMap(String language) {
        HashMap<Long,BaseMenuDto> menuMap;
        final String mapCache = stringRedisTemplate.opsForValue().get(PILE_BASE_EXPLANATORY_MENU_MAP_CACHE);
        if (StringUtils.isNotEmpty(mapCache)) {
            log.info(">>>mapCache cache");
            TypeReference<HashMap<Long, BaseMenuDto>> typeReference = new TypeReference<HashMap<Long, BaseMenuDto>>() {};
            menuMap = JSON.parseObject(mapCache, typeReference);
        } else {
            log.info(">>>call for menu");
            final Result<List<? extends BaseMenuDto>> chargebusi = authorityManagerFeign.listMenu("Chargebusi", null, language);
            final HashMap<Long, BaseMenuDto> finalMenuMap = new HashMap<>();
            chargebusi.getData().forEach(item -> {
                BaseMenuDto menu = new BaseMenuDto();
                menu.setMenuName(item.getMenuName());
                menu.setWebMenuPath(item.getWebMenuPath());
                finalMenuMap.putIfAbsent(item.getId(), menu);
                if (CollUtil.isNotEmpty(item.getChildren())) {
                    item.getChildren().forEach(child -> {
                        BaseMenuDto childMenu = new BaseMenuDto();
                        childMenu.setMenuName(child.getMenuName());
                        childMenu.setWebMenuPath(child.getWebMenuPath());
                        finalMenuMap.putIfAbsent(child.getId(), childMenu);
                    });
                }
            });
            menuMap = finalMenuMap;
            stringRedisTemplate.opsForValue().set(PILE_BASE_EXPLANATORY_MENU_MAP_CACHE, JSON.toJSONString(menuMap), 1, TimeUnit.MINUTES);
        }
        return menuMap;
    }
}
