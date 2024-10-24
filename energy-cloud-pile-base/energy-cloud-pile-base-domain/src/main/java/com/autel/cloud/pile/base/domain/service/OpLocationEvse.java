package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.model.PileQrCodeDTO;
import com.autel.cloud.pile.base.dto.OpEvseInfoDTO;
import com.autel.cloud.pile.base.dto.OpPileEvseDTO;
import com.autel.cloud.pile.base.dto.PilePageDTO;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.location.PromptVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;

import java.util.List;

/**
 * @ClassName OpLocationEvse
 * @Author A22121
 * @Description
 * @Date 2022/5/11 9:16
 * @Version 0.0.1-SNAPSHOT
 */
public interface OpLocationEvse {
    /**
     * 查询桩信息
     *
     * @param pileSn
     * @return
     */
    Result<OpPileEvseInfoVO> getPileInfoByPileSn(String pileSn);

    /**
     * 查询桩信息列表
     *
     * @param pileSnList
     * @return
     */
    Result<List<OpPileEvseInfoVO>> getPileInfoByPileSnList(List<String> pileSnList);

    /**
     * 查询枪列表
     *
     * @param pileSn
     * @return
     */
    com.autel.cloud.base.model.Result<List<OpEvseInfoVO>> getEvseListByPileSn(String pileSn);

    Boolean checkExist(OpPileEvseDTO dto);

    /**
     * 场站详情桩分页查询
     *
     * @param pilePageDTO 场站id
     * @return 桩分院
     */
    Result<Page<PilePageVO>> stationPilePage(PilePageDTO pilePageDTO);

    /**
     * 查询桩信息
     *
     * @param evseSn
     * @return
     */
    Result<OpEvseInfoDTO> getPileInfoByEvseSn(String evseSn);

    /**
     * 查询桩信息
     *
     * @param pileName   桩名称
     * @param locationId 场站主键
     * @return 查询桩信息
     */
    Result<List<OpLocationPileEvseVO>> getPileInfoByPileNameAndLocationId(String pileName, Long locationId);

    /**
     * 删除桩信息
     *
     * @param pileId
     * @param deleteFlag 是否同步删除运维桩
     */
    Result<Boolean> deleteByPileId(Long pileId, Boolean deleteFlag);

    /**
     * 删除充电桩前的必校验
     * @param pileId
     * @return
     */
    void checkPileIsCanDel(Long pileId);

    /**
     * 删除场站设备提示校验
     * @param pileSn
     */
    PromptVO checkPilePrompt(String pileSn);

    /**
     * 合并必须和提示校验
     * @param pileId
     * @return
     */
    PromptVO checkPileDelPrompt(Long pileId);


    /**
     * 桩详情
     *
     * @param pileId
     * @return
     */
    com.autel.cloud.base.model.Result<OpPileEvseInfoVO> detail(Long pileId);

    Result<OpPileEvseInfoVO> detailBySn(String sn);

    /**
     * 桩-运行-图表信息查询
     * @param pileSn
     * @return
     */
    com.autel.cloud.base.model.Result<List<OpEvseInfoVO>> getHistoryMeterInfo(String pileSn);

    /**
     * 根据桩sn查询启用禁用列表 如果不为不可用状态，就是可以禁用
     * @param pileSn
     * @return
     */
    List<OpEvseEnableVO> getAbleDisAbleList(String pileSn);

    /**
     * 校验桩下边的所有枪是否可以禁用
     * @param pileSn
     * @return
     */
    boolean checkAllEvseEnable(String pileSn);

    /**
     * 分页查询批量下载桩二维码页面桩信息
     *
     * @param keyWord    关键词
     * @param locationId 场站ID
     * @param page
     * @param pageSize
     * @return {@link Page}<{@link PileQrCodeDTO}>
     */
    Page<PileQrCodeDTO> getPileQrCodePage(String keyWord, String locationId, int page, int pageSize);

    OpPileEvseInfoVO getPileDetailByPileId(Long pileId);

}
