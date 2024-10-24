package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.repository.OpLocationRepository;
import com.autel.cloud.pile.base.domain.service.DataStatsService;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.autel.cloud.pile.base.infrastructure.mapper.DataStatsMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import com.autel.cloud.pile.base.vo.EvseForStatsVO;
import com.autel.cloud.pile.base.vo.EvseStatusStatisticVO;
import com.autel.cloud.pile.base.vo.GunStatsVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * @Author A22282
 * @Date 2022/4/27 16:59
 */
@Service
@Slf4j
public class DataStatsServiceImpl implements DataStatsService {
    @Autowired
    private DataStatsMapper dataStatsMapper;
    @Autowired
    private OpLocationRepository opLocationRepository;
    @Resource
    @Qualifier("redisTemplates")
    private RedisTemplate<String, Object> redisTemplate;

    @Override
    public Result<EvseForStatsVO> evseStats(Long operatorId) {
        log.info("evseStats,operatorId={}", operatorId);
        //按power_type类型统计
        List<Map<String, Object>> mapList = dataStatsMapper.evseStats(operatorId);
        //场站数
        Integer locationCount = dataStatsMapper.countByOperatorId(operatorId);
        log.info("evseStats,mapList={},locationCount={}", mapList, locationCount);
        List<EvseForStatsVO.EvseStats> evseStats = EvseForStatsVO.getList(mapList);
        return Result.ofSucceed(EvseForStatsVO.builder().evseStatsList(evseStats).locationCount(locationCount).build());
    }

    @Override
    public Result<List<GunStatsVO>> gunStats(Long operatorId) {
        log.info("gunStats,operatorId={}", operatorId);
        //根据场站从redis读取数据
        List<OpLocationEntity> list = opLocationRepository.list(new QueryWrapper<OpLocationEntity>().select("id").eq("deleted", 0).eq("operator_id", operatorId));
        if (list == null || list.isEmpty()) {
            log.info("gunStats,operatorId={},该商户下无场站", operatorId);
            return Result.ofSucceed();
        }
        List<GunStatsVO> gunStatsVOList = new ArrayList<>();
        try {
            list.forEach(entity -> {
                String locationEvseStateKey = RedisKeyConstant.getHashPileBaseLocationEvseState(entity.getId());
                Map<Object, Object> entries = redisTemplate.opsForHash().entries(locationEvseStateKey);
                entries.entrySet().forEach(e -> {
                    int status = (int) e.getValue();
                    GunStatsVO.setList(EvseDeviceStatusEnum.getEnumByCode(status), gunStatsVOList);
                });
            });
            return Result.ofSucceed(gunStatsVOList);
        } catch (Exception e) {
            log.error("gunStats,exception={}", e.getMessage());
        }
        //查询数据库
        log.info("gunStats,无法从redis查询结果，查询数据库，operatorId={}", operatorId);
        List<Map<String, Object>> mapList = dataStatsMapper.statusCountByOperatorId(operatorId);
        log.info("gunStats,mapList={}", mapList);
        return Result.ofSucceed(GunStatsVO.getList(mapList));
    }

    @Override
    public Result<List<GunStatsVO>> getAllGunStats() {
        log.info("getAllGunStats,查询所有商户枪状态");
        String key = "pile-base:dataStatsServiceImpl:allGunStats";
        List<GunStatsVO> gunStatsVOList = (List<GunStatsVO>) redisTemplate.opsForValue().get(key);
        if (gunStatsVOList != null && !gunStatsVOList.isEmpty()) {
            return Result.ofSucceed(gunStatsVOList);
        }
        //查询所有商户
        List<Map<String, Object>> mapList = dataStatsMapper.statusCountByOperatorId(null);
        List<GunStatsVO> resultList = GunStatsVO.getList(mapList);
        if (!resultList.isEmpty()) {
            //设置缓存
            redisTemplate.opsForValue().set(key, resultList, 30, TimeUnit.MINUTES);
        }
        return Result.ofSucceed(resultList);
    }

    @Override
    public Result<EvseForStatsVO> getAllEvseStats() {
        log.info("getAllEvseStats,查询所有充电设施统计数量");
        String key = "pile-base:dataStatsServiceImpl:allEvseStats";
        EvseForStatsVO evseForStatsVO = (EvseForStatsVO) redisTemplate.opsForValue().get(key);
        if (evseForStatsVO != null) {
            return Result.ofSucceed(evseForStatsVO);
        }
        //按power_type类型统计
        List<Map<String, Object>> mapList = dataStatsMapper.evseStats(null);
        //场站数
        Integer locationCount = dataStatsMapper.countByOperatorId(null);
        log.info("getAllEvseStats,mapList={},locationCount={}", mapList, locationCount);
        List<EvseForStatsVO.EvseStats> evseStats = EvseForStatsVO.getList(mapList);
        EvseForStatsVO result = EvseForStatsVO.builder().evseStatsList(evseStats).locationCount(locationCount).build();
        if (!evseStats.isEmpty() && locationCount > 0) {
            //设置缓存
            redisTemplate.opsForValue().set(key, result, 30, TimeUnit.MINUTES);
        }
        return Result.ofSucceed(result);
    }
}
