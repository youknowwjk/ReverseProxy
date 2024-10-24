package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.AgreementFunctionDetailDto;
import com.autel.cloud.pile.base.dto.BusinessReqDto;
import com.autel.cloud.pile.base.dto.BusinessRespDto;
import com.autel.cloud.pile.base.dto.UnUsedLicenceInfoDto;
import com.autel.cloud.pile.base.dto.subscribe.*;
import com.autel.cloud.pile.base.dto.subscribe.LicenceGracePeriodExtendedDto;
import com.autel.cloud.pile.base.vo.AgreementFunctionReqVo;
import com.autel.cloud.pile.base.vo.license.LicenseDetailVO;

import java.util.List;

/**
 * @description 订阅需求权益相关接口
 * @auther A23204
 * @datetime 2023/6/13 16:04
 */
public interface SubscribePileRightsService {

    String HAVE_CHARGE_RIGHTS_KEY = "haveChargeRight:";

    List<AgreementFunctionDetailDto> getHaveRightsPileList(AgreementFunctionReqVo agreementFunctionReqVo);

    Result<Boolean> pileIsAvailable(String pileSn);

    Result<List<UnUsedLicenceInfoDto>> queryUnUsedLicence(String skuCode, Long merchantId);

    Result<Long> pileRightsWithExtension(String pileSn);

    Result<List<UnUsedLicenceInfoDto>> queryLicence(String sellerId);

    BusinessRespDto findSellerIdByLicenseNum(BusinessReqDto businessReqDto);

    /**
     * description: extendGracePeriod 统一延长licence 宽限期
     * version: 1.0
     * date: 2023/12/19 10:24
     * author: A23204
     *
     * @param LicenceGracePeriodExtendedDto
     * @return com.autel.cloud.base.http.pojo.Result<java.lang.Boolean>
     */
    Result<List<String>> extendGracePeriod(LicenceGracePeriodExtendedDto LicenceGracePeriodExtendedDto);

    Result<List<SubscribeBenefitMetaDto>> benefitConfigQuery();

    Result<List<SubscribeFuncConfDto>> functionConfigQuery();

    Result<List<PileBenefitFunctionRespDto>> getPileFunction(BenefitFunctionReqDto benefitFunctionReqDto);

    Result<Boolean> functionEdit(FunctionModifyDto functionModifyDto);

    Result<Boolean> benefitEdit(BenefitEditDto benefitEditDto);

    Result<List<BenefitFunctionRelationDto>> benefitFuncList();

    /**
     * description: getFunctionListByBenefitId 根据权益id，查询功能控制点集合。有缓存
     * version: 1.0
     * date: 2023/12/7 16:05
     * author: A23204
     *
     * @param benefitId
     * @return java.util.List<java.lang.String>
     */
    List<String> getFunctionListByBenefitId(String benefitId);

    /**
     * description: getFunctionBenefitList 根据功能控制点返回返回服务权益结合
     * version: 1.0
     * date: 2023/12/11 11:37
     * author: A23204
     *
     * @param functionId
     * @return java.util.List<java.lang.String>
     */
    List<String> getFunctionBenefitList(String functionId);

    void reSetFunctionIdBenefitList();

    void updateServiceIdBySellerId(String sellerId);

    /**
     * description: functionAvailableList 查询桩生效中的功能控制点
     * version: 1.0
     * date: 2024/1/23 9:47 
     * author: A23204 
     * 
     * @param benefitAvailableFuncListReqDto
     * @return com.autel.cloud.base.http.pojo.Result<List<BenefitAvailableFuncListRespDto>>
     */ 
    Result<List<BenefitAvailableFuncListRespDto>> functionAvailableList(BenefitAvailableFuncListReqDto benefitAvailableFuncListReqDto);

    /**
     * description: pileWithGunIsAvailable  根据桩+枪，判断是否有充电权限
     * version: 1.0
     * date: 2024/6/5 17:07 
     * author: A23204 
     * 
     * @param pileSn
     * @param gunNum
     * @return com.autel.cloud.base.http.pojo.Result<java.lang.Boolean>
     */ 
    Result<Boolean> pileWithGunIsAvailable(String pileSn, Integer gunNum);
}
