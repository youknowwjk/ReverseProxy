package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.pile.base.domain.repository.OpLocationPriceDetailRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationPriceRepository;
import com.autel.cloud.pile.base.domain.service.OpLocationPriceService;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.autel.cloud.pile.base.dto.LocationPriceDTO;
import com.autel.cloud.pile.base.dto.LocationPriceDetailDTO;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPriceDetailEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPriceEntity;
import com.autel.cloud.pile.base.vo.OpLocationPriceDetailVO;
import com.autel.cloud.pile.base.vo.OpLocationPriceInfoVo;
import com.autel.cloud.pile.base.vo.OpLocationPriceVO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author temp
 * @description 针对表【op_location_price(成本电价表)】的数据库操作Service实现
 * @createDate 2023-03-07 14:48:13
 */
@Service
@Slf4j
public class OpLocationPriceServiceImpl implements OpLocationPriceService {

    private final OpLocationPriceRepository opLocationPriceRepository;
    @Autowired
    private OpLocationPriceDetailRepository opLocationPriceDetailRepository;
    @Autowired
    private OpLocationService opLocationService;

    public OpLocationPriceServiceImpl(OpLocationPriceRepository opLocationPriceRepository) {
        this.opLocationPriceRepository = opLocationPriceRepository;
    }

    @Override
    @Transactional
    public Long add(LocationPriceDTO dto) {
        log.info("add,dto={}", JSON.toJSONString(dto));
        /*if (this.checkName(dto)) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_PRICE_NAME_EXIST);
        }*/
        if (this.checkParams(dto)) {
            throw new IllegalArgumentException();
        }
        OpLocationPriceEntity entity = this.toLocationPriceEntity(dto);
        boolean save = opLocationPriceRepository.insert(entity);
        log.info("add,save={}", save);
        Long id = entity.getId();
        List<OpLocationPriceDetailEntity> entityList = this.toLocationPriceDetail(dto, id);
        boolean insertBatch = opLocationPriceDetailRepository.insertBatch(entityList);
        log.info("add,insertBatch={}", insertBatch);
        return id;
    }

    @Override
    @Transactional
    public Long edit(LocationPriceDTO dto) {
        log.info("edit,dto={}", JSON.toJSONString(dto));
        Long id = dto.getId();
        OpLocationPriceEntity entity = opLocationPriceRepository.findOne(id);
        if (entity == null) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }
        if (this.checkParams(dto)) {
            throw new IllegalArgumentException();
        }
        BeanUtils.copyProperties(entity, dto);
        boolean update = opLocationPriceRepository.updateEntity(entity);
        log.info("edit,update={}", update);
        List<OpLocationPriceDetailEntity> entityList = this.toLocationPriceDetail(dto, id);
        boolean updateList = opLocationPriceDetailRepository.updateList(entityList, id);
        log.info("edit,updateList={}", updateList);
        return id;
    }

    @Override
    public OpLocationPriceVO detail(Long id) {
        log.info("detail,id={}", id);
        OpLocationPriceEntity entity = opLocationPriceRepository.findOne(id);
        if (entity != null) {
            List<OpLocationPriceDetailEntity> entityList = opLocationPriceDetailRepository.findList(id);
            OpLocationPriceVO priceVO = this.toLocationPriceVO(entity, entityList);
            return priceVO;
        }
        return null;
    }

    @Override
    @Transactional
    public Long delete(Long id) {
        log.info("delete,id={}", id);
        OpLocationPriceEntity entity = opLocationPriceRepository.findOne(id);
        if (entity != null) {
            boolean delete = opLocationPriceRepository.delete(id);
            log.info("delete,delete={}", delete);
            if (delete) {
                int count = opLocationPriceDetailRepository.deleteBatch(id);
                log.info("delete,count={}", count);
                return id;
            }
        }
        return null;
    }

    @Override
    public List<OpLocationPriceVO> getList(Long locationId) {
        Long sellerId = this.getSellerId(locationId);
        log.info("getList,locationId={},sellerId={}", locationId, sellerId);
        List<OpLocationPriceEntity> priceEntityList = opLocationPriceRepository.findList(locationId, sellerId);
        if (!CollectionUtils.isEmpty(priceEntityList)) {
            List<OpLocationPriceVO> priceVoList = new ArrayList<>(priceEntityList.size());

            List<Long> priceIds = priceEntityList.stream().map(OpLocationPriceEntity::getId).collect(Collectors.toList());
            List<OpLocationPriceDetailEntity> priceDetailEntityList = opLocationPriceDetailRepository.findList(priceIds);
            Map<Long, List<OpLocationPriceDetailEntity>> priceDetailEntityMap = priceDetailEntityList.stream().collect(Collectors.groupingBy(OpLocationPriceDetailEntity::getPriceId));

            priceEntityList.stream().forEach(price -> {
                priceVoList.add(this.toLocationPriceVO(price, priceDetailEntityMap.get(price.getId())));
            });
            return priceVoList;
        }
        return null;
    }

    private Long getSellerId(Long locationId) {
        OpLocationElasticDTO dto = opLocationService.findById(locationId);
        if (dto != null) {
            return dto.getOperatorId();
        }
        return null;
    }

    @Override
    @Transactional
    public int deleteByLocationId(Long id) {
        Long sellerId = LoginUserHolder.getLoginUser().getPayload().getSellerId();
        log.info("deleteByLocationId,locationId={},sellerId={}", id, sellerId);
        List<OpLocationPriceEntity> entityList = opLocationPriceRepository.findList(id, sellerId);
        if (!CollectionUtils.isEmpty(entityList)) {
            List<Long> ids = entityList.stream().map(OpLocationPriceEntity::getId).collect(Collectors.toList());
            int batch = opLocationPriceRepository.deleteBatch(ids);
            log.info("deleteByLocationId,batch={}", batch);
            if (batch > 0) {
                int deleteBatch = opLocationPriceDetailRepository.deleteBatch(ids);
                log.info("deleteByLocationId,deleteBatch={}", deleteBatch);
                return batch;
            }
        }
        return 0;
    }

    @Override
    public OpLocationPriceInfoVo getPriceList(Long locationId) {
        Long sellerId = this.getSellerId(locationId);
        log.info("getPriceList,locationId={},sellerId={}", locationId, sellerId);
        OpLocationPriceInfoVo infoVo = new OpLocationPriceInfoVo();
        OpLocationElasticDTO locationDto = opLocationService.findById(locationId);
        infoVo.setLocationId(locationDto.getId());
        infoVo.setLocationName(locationDto.getName());
        infoVo.setPriceFlag(locationDto.getPriceFlag());
        infoVo.setPriceVoList(this.getList(locationId));
        return infoVo;
    }

    private OpLocationPriceVO toLocationPriceVO(OpLocationPriceEntity entity, List<OpLocationPriceDetailEntity> entityList) {
        OpLocationPriceVO priceVO = new OpLocationPriceVO();
        BeanUtils.copyProperties(entity, priceVO);
        if (!CollectionUtils.isEmpty(entityList)) {
            Map<String, List<OpLocationPriceDetailEntity>> priceDetailMap = entityList.stream().collect(Collectors.groupingBy(OpLocationPriceDetailEntity::getDays));
            List<OpLocationPriceDetailVO> priceDetailVoList = new ArrayList<>(priceDetailMap.size());
            priceDetailMap.forEach((k, v) -> {
                OpLocationPriceDetailVO detailVO = new OpLocationPriceDetailVO();
                OpLocationPriceDetailEntity detailEntity = v.get(0);
                BeanUtils.copyProperties(detailEntity, detailVO);
                detailVO.setDays(this.getArrayDays(k));
                detailVO.setDayPriceList(this.getDayPriceVO(v));
                priceDetailVoList.add(detailVO);
            });
            priceVO.setPriceDetails(priceDetailVoList);
        }
        return priceVO;
    }

    private List<OpLocationPriceDetailVO.DayPriceVO> getDayPriceVO(List<OpLocationPriceDetailEntity> entityList) {
        return entityList.stream().map(entity -> {
            OpLocationPriceDetailVO.DayPriceVO vo = new OpLocationPriceDetailVO.DayPriceVO();
            BeanUtils.copyProperties(entity, vo);
            return vo;
        }).collect(Collectors.toList());
    }

    private List<Integer> getArrayDays(String days) {
        if (StringUtils.hasText(days)) {
            List<Integer> temp = Arrays.stream(days.split(",")).map(d -> Integer.valueOf(d)).collect(Collectors.toList());
            return temp;
        }
        return null;
    }

    private List<OpLocationPriceDetailEntity> toLocationPriceDetail(LocationPriceDTO dto, Long id) {
        List<LocationPriceDetailDTO> priceDetails = dto.getPriceDetails();
        List<OpLocationPriceDetailEntity> resultEntityList = new ArrayList<>();
        priceDetails.stream().forEach(detail -> {
            List<LocationPriceDetailDTO.DayPriceDTO> dayPriceList = detail.getDayPriceList();
            dayPriceList.stream().forEach(dayPriceDTO -> {
                OpLocationPriceDetailEntity entity = new OpLocationPriceDetailEntity();
                BeanUtils.copyProperties(detail, entity);
                BeanUtils.copyProperties(dayPriceDTO, entity);
                entity.setPriceId(id);
                entity.setDays(this.getStringDays(detail.getDays()));
                //复制属性同时会把ID也复制过来
                entity.setId(null);
                resultEntityList.add(entity);
            });
        });
        return resultEntityList;
    }

    private String getStringDays(List<Integer> days) {
        if (!CollectionUtils.isEmpty(days)) {
            String temp = days.stream().map(d -> d.toString()).collect(Collectors.joining(","));
            return temp;
        }
        return null;
    }

    private OpLocationPriceEntity toLocationPriceEntity(LocationPriceDTO dto) {
        OpLocationPriceEntity entity = new OpLocationPriceEntity();
        BeanUtils.copyProperties(dto, entity);
        return entity;
    }

    private boolean checkParams(LocationPriceDTO dto) {
        List<LocationPriceDetailDTO> priceDetails = dto.getPriceDetails();
        if (CollectionUtils.isEmpty(priceDetails)) {
            return true;
        }
        return false;
    }

    private boolean checkName(LocationPriceDTO dto) {
        List<OpLocationPriceEntity> priceEntityList = opLocationPriceRepository.getByName(dto);
        if (!CollectionUtils.isEmpty(priceEntityList)) {
            return true;
        }
        return false;
    }
}




