package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.convert.Convert;
import cn.hutool.json.JSONUtil;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.convert.OpEvseTypeConvert;
import com.autel.cloud.pile.base.domain.repository.OpEvseTypeRepository;
import com.autel.cloud.pile.base.domain.repository.OpImageRepository;
import com.autel.cloud.pile.base.domain.service.OpEvseTypeService;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseTypeEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpImageEntity;
import com.autel.cloud.pile.base.vo.OpEvseTypeVO;
import com.autel.cloud.pile.base.vo.app.GunTypeIconRespDTO;
import com.autel.cloud.pile.base.vo.app.GunTypeRespDTO;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.CommandLineRunner;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@Log4j2
public class OpEvseTypeServiceImpl implements OpEvseTypeService,CommandLineRunner {

    @Autowired
    private OpEvseTypeRepository opEvseTypeRepository;

    @Autowired
    private OpImageRepository opImageRepository;

    @Autowired
    @Qualifier("redisTemplates")
    private RedisTemplate<String,Object> redisTemplate;


    @Override
    public Result<List<OpEvseTypeVO>> list() {
        return Result.ofSucceed(OpEvseTypeConvert.toEvseTypeVOs(getEvseTypeList()));
    }

    @Override
    public Result<List<GunTypeRespDTO>> getGunType() {

        Map<Long, OpImageEntity> imageEntityMap = getImageEntityMap();
        //根据枪类型进行分组
        Set<Map.Entry<Integer, List<OpEvseTypeEntity>>> entries = getEvseTypeList()
                .stream().collect(Collectors.groupingBy(OpEvseTypeEntity::getGunType)).entrySet();

        List<GunTypeRespDTO> collect = entries.stream().map(entity -> {
            List<GunTypeIconRespDTO> gunTypeIconRespDtos = entity.getValue().stream().map(a -> GunTypeIconRespDTO.builder()
                    .type(Convert.toInt(a.getType()))
                    .imageId(a.getImageId())
                    .imageUrl(imageEntityMap.get(a.getImageId()).getUrl()).build()).collect(Collectors.toList());
            OpEvseTypeEntity opEvseTypeEntity = (OpEvseTypeEntity) ((List) entity.getValue()).get(0);

            GunTypeRespDTO gunTypeRespDTO = new GunTypeRespDTO();
            gunTypeRespDTO.setId(Convert.toLong(entity.getKey()));
            gunTypeRespDTO.setName(opEvseTypeEntity.getName());
            gunTypeRespDTO.setGunType(Convert.toInt(opEvseTypeEntity.getGunType()));
            gunTypeRespDTO.setIcon(gunTypeIconRespDtos);
            return gunTypeRespDTO;
        }).collect(Collectors.toList());
        log.info("getGunType.collect = {}", JSONUtil.toJsonStr(collect));
        return Result.ofSucceed(collect);
    }

    @Override
    public void run(String... args) throws Exception {
        List<OpEvseTypeEntity> evseTypeList = opEvseTypeRepository.list();
        if (!CollectionUtils.isEmpty(evseTypeList)) {
            redisTemplate.opsForValue().set(RedisKeyConstant.getStringEvseTypeList(), evseTypeList);
        }
        //通过图片id获取所有的枪图片信息
        List<Long> imageIds = evseTypeList.stream().map(OpEvseTypeEntity::getImageId).collect(Collectors.toList());
        List<OpImageEntity> imageEntities = opImageRepository.getImagesByIds(imageIds);
        Map<String, OpImageEntity> imageEntityMap = imageEntities.stream().collect(Collectors.toMap(e -> String.valueOf(e.getId()), Function.identity()));
        if (!CollectionUtils.isEmpty(imageEntityMap)) {
            redisTemplate.opsForHash().putAll(RedisKeyConstant.getHashImageMap(), imageEntityMap);
        }
    }

    @Override
    public Result<Boolean> syncGunType() {
        try {
            this.run(null);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        return Result.ofSucceed(true);
    }

    private List<OpEvseTypeEntity> getEvseTypeList() {
        List<OpEvseTypeEntity> evseTypeList = (List<OpEvseTypeEntity>) redisTemplate.opsForValue().get(RedisKeyConstant.getStringEvseTypeList());
        return evseTypeList;
    }

    private Map<Long, OpImageEntity> getImageEntityMap() {
        Map<Object, Object> entries = redisTemplate.opsForHash().entries(RedisKeyConstant.getHashImageMap());
        if (!CollectionUtils.isEmpty(entries)) {
            Map<Long, OpImageEntity> imageEntityMap = new HashMap<>(entries.size());
            entries.forEach((k, v) -> {
                Long id = Long.parseLong(k.toString());
                OpImageEntity entity = (OpImageEntity) v;
                imageEntityMap.put(id, entity);
            });
            return imageEntityMap;
        }
        return new HashMap<>();
    }
}
