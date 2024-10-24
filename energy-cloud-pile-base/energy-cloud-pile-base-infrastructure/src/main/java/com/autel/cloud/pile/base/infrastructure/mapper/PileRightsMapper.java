package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.dto.AgreementFunctionDetailDto;
import com.autel.cloud.pile.base.dto.subscribe.BenefitFunctionReqDto;
import com.autel.cloud.pile.base.dto.subscribe.PileBenefitFunctionRespDto;
import com.autel.cloud.pile.base.dto.subscribe.UnusedLicenceQueryParam;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbLenBindRelationEntity;
import com.autel.cloud.pile.base.vo.AgreementFunctionReqVo;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Repository;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

/**
 * @description
 * @auther A23204
 * @datetime 2023/6/13 21:56
 */
@Repository
public interface PileRightsMapper {

    List<AgreementFunctionDetailDto> getHaveRightsPileList(@Param("agreementFunctionReqVo") AgreementFunctionReqVo agreementFunctionReqVo);

    void updatePendingStatus(@Param("currentTimestamps") Long currentTimestamps);

    void updateExpiredOtherRights(@Param("currentTimestamps") Long currentTimestamps,  @Param("functionBenefitList") List<String> functionBenefitList);

    void agreementStatus(@Param("orderId") String orderId);

    List<TbLenBindRelationEntity> selectUnBindLicence(@Param("unusedLicenceQueryParam") UnusedLicenceQueryParam unusedLicenceQueryParam);

    //List<AgreementFunctionDetailDto> getHaveFunctionPileList(@Param("benefitFunctionReqDto") BenefitFunctionReqDto benefitFunctionReqDto);
}
