package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.pile.base.dto.meter.PageQueryDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.LocationMeterEntity;
import com.autel.cloud.pile.base.vo.LocationMeterVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * @author temp
 * @description 针对表【op_location_meter(电表)】的数据库操作Service
 * @createDate 2023-01-29 17:26:48
 */
public interface LocationMeterRepository extends IService<LocationMeterEntity> {

    /**
     * @param sellerId
     * @param meterIdList
     * @param searchValue
     * @return
     * @function 根据条件获取商家下的电表信息
     */
    List<LocationMeterEntity> getList(Long sellerId, List<Long> meterIdList, String searchValue);

    /**
     * @param locationMeterEntity
     * @return
     * @function 添加电表
     */
    boolean add(LocationMeterEntity locationMeterEntity);

    /**
     * @param locationMeterEntity
     * @return
     * @function 电表名称校验
     */
    boolean checkMeterNameUnique(LocationMeterEntity locationMeterEntity);

    /**
     * @return
     * @function 逻辑删除电表
     */
    boolean logicDelete(Long id);

    /**
     * @param pageQueryDTO
     * @return
     * @function 根据条件查询电表信息
     */
    List<LocationMeterEntity> queryPages(PageQueryDTO pageQueryDTO);

    /**
     * @param locationMeterEntity
     * @return
     * @function 电表SN校验
     */
    boolean checkSnUnique(LocationMeterEntity locationMeterEntity);

    List<LocationMeterEntity> findListBySn(List<String> sns);

    List<LocationMeterEntity> getByMeterIds(List<Long> meterIds);
}
