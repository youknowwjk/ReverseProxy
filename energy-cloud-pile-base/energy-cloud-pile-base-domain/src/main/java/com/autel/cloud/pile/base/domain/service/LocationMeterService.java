package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.dto.LocationMeterDTO;
import com.autel.cloud.pile.base.dto.meter.MeterIdsQueryDTO;
import com.autel.cloud.pile.base.dto.meter.MeterQueryDTO;
import com.autel.cloud.pile.base.dto.meter.PageParamDTO;
import com.autel.cloud.pile.base.dto.meter.PageQueryDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.LocationMeterEntity;
import com.autel.cloud.pile.base.vo.LocationMeterVO;
import com.autel.cloud.pile.base.vo.MeterVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;

import java.util.List;

/**
 * @author temp
 * @description 针对表【op_location_meter(电表)】的数据库操作Service
 * @createDate 2023-01-29 17:26:48
 */
public interface LocationMeterService {

    /**
     * @return
     * @function 查询商家可以给智能充电群组绑定的电表列表(过滤掉已经被绑定过的)
     */
    List<LocationMeterVO> getList(MeterQueryDTO meterQueryDTO);

    /**
     * @param locationMeterDTO
     * @return
     * @function 新增电表
     */
    Long add(LocationMeterDTO locationMeterDTO);

    /**
     * @param locationMeterDTO
     * @return
     * @function 更新电表
     */
    Boolean update(LocationMeterDTO locationMeterDTO);

    /**
     * @param id
     * @return
     * @function 删除电表
     */
    Boolean delete(Long id);

    /**
     * @param id
     * @return
     * @function 查询电表信息详情
     */
    LocationMeterVO detail(Long id);

    /**
     * @param pageQueryDTO
     * @return
     * @function 分页查询商家下的电表数据
     */
    Page<LocationMeterVO> queryPages(PageQueryDTO pageQueryDTO);

    /**
     * 查询商户所有的电表记录ID
     * @return
     */
    List<Long> selectIdListBySellerId(Long sellerId);

    String generateDefaultMeterName(Integer brandEnum);

    List<LocationMeterVO> getIds(List<String> sns);

    List<LocationMeterEntity> findListBySn(List<String> sns);

    List<LocationMeterEntity> getByMeterIds(List<Long> meterIds);

    Page<MeterVO> queryByPage(PageParamDTO paramDTO);

    List<MeterVO> queryMetetByMetetIds(MeterIdsQueryDTO paramDTO);
}
