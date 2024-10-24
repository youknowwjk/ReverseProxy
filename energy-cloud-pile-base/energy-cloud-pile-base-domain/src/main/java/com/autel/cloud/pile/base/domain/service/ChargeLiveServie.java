package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.pile.base.dto.OpLocationEvseStateCountDTO;
import com.autel.cloud.pile.base.dto.OpLocationLiveEvsePageDTO;
import com.autel.cloud.pile.base.dto.OpLocationLiveEvseViewDTO;
import com.autel.cloud.pile.base.dto.pos.DeviceInfoDTO;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.pos.DeviceInfoVO;

import java.util.List;

public interface ChargeLiveServie {

    /**
     * 枪状态统计
     *
     * @param opLocationEvseStateCountDTO
     * @return
     */
    List<OpLocationEvseStateCountVO> countEvseByState(OpLocationEvseStateCountDTO opLocationEvseStateCountDTO);

    /**
     * 查询充电实况视图
     *
     * @param opLocationLiveEvseViewDTO
     * @return
     */
    List<OpLocationLiveEvseViewVO> listLiveEvseView(OpLocationLiveEvseViewDTO opLocationLiveEvseViewDTO);

    /**
     * 充电实况枪更多信息
     *
     * @param evseSn
     * @return
     */
    OpLocationLiveEvseMoreVO getEvseLiveInfo(String evseSn);

    /**
     * 充电实况列表查询
     *
     * @param opLocationLiveEvsePageDTO
     * @return
     */
    PageVO<OpLocationLiveEvseVO> listLiveEvseByPage(OpLocationLiveEvsePageDTO opLocationLiveEvsePageDTO);

    /**
     * 查询桩详情中的充电实况
     * @param evseSn
     * @return
     */
    OpLocationLiveEvseCurrentVO getCurrentStatus(String evseSn);

    /**
     * 按枪状态统计所有场站
     * @return
     */
    List<OpLocationEvseStateCountVO> countAllEvseByState(List<Long> locationIds);

    List<DeviceInfoVO> selectDeviceInfo(List<DeviceInfoDTO> deviceInfoDTOList);
}
