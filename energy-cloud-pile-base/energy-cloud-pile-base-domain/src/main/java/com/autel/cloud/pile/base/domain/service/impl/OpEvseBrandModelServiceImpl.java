package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.base.common.enums.ResultCodeEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.repository.OpEvseBrandModelRepository;
import com.autel.cloud.pile.base.domain.service.OpEvseBrandModelService;
import com.autel.cloud.pile.base.dto.OpEvseBrandModelDTO;
import com.autel.cloud.pile.base.dto.VerifyBrandNameAndProductModelDTO;
import com.autel.cloud.pile.base.vo.OpEvseBrandModelVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.List;

/**
 * <p>
 * 桩品牌型号 实现类
 * </p>
 *
 * @author A22587
 * @since 2022-09-13
 */

@Service
@Log4j2
public class OpEvseBrandModelServiceImpl implements OpEvseBrandModelService {

    @Autowired
    private OpEvseBrandModelRepository opEvseBrandModelRepository;

    /**
     * 产品信息录入
     */
    @Override
    public Result<OpEvseBrandModelVO> addOpEvseBrandModel(OpEvseBrandModelDTO opEvseBrandModelDTO) {
        log.info(BaseConstant.ADDOPEVSEBRANDMODEL_OPEVSEBRANDMODELDTO, opEvseBrandModelDTO);
        if (ObjectUtils.isEmpty(opEvseBrandModelDTO)) {
            return Result.ofFailed(ResultCodeEnum.BAD_REQUEST.setMessage(BaseConstant.PARAM_IS_NULL));
        }
        return opEvseBrandModelRepository.addOpEvseBrandModel(opEvseBrandModelDTO);
    }

    /**
     * 分页查询产品型号信息
     */
    @Override
    public Result<Page<OpEvseBrandModelVO>> pages(OpEvseBrandModelDTO opEvseBrandModelDTO) {
        log.info(BaseConstant.ADDOPEVSEBRANDMODEL_OPEVSEBRANDMODELDTO, opEvseBrandModelDTO);
        if (ObjectUtils.isEmpty(opEvseBrandModelDTO)) {
            return Result.ofFailed(ResultCodeEnum.BAD_REQUEST.setMessage(BaseConstant.PARAM_IS_NULL));
        }
        return opEvseBrandModelRepository.pages(opEvseBrandModelDTO);
    }

    /**
     *列表查询（根据品牌条件）
     */
    @Override
    public Result<List<OpEvseBrandModelVO>> queryOpEvseBrandModelByBrand(Long brandId) {
        if (StringUtils.isEmpty(brandId)) {
            return Result.ofFailed(ResultCodeEnum.BAD_REQUEST.setMessage(BaseConstant.PARAM_IS_NULL));
        }
        return opEvseBrandModelRepository.queryOpEvseBrandModelByBrand(brandId);
    }

    /**
     *产品型号详情
     */
    @Override
    public Result<OpEvseBrandModelVO> opEvseBrandModelDetail(Long id) {
        if (StringUtils.isEmpty(id)) {
            return Result.ofFailed(ResultCodeEnum.BAD_REQUEST.setMessage(BaseConstant.PARAM_IS_NULL));
        }
        return opEvseBrandModelRepository.opEvseBrandModelDetail(id);
    }

    /**
     *编辑产品型号信息
     */
    @Override
    public Result<Boolean> updateOpEvseBrandModel(OpEvseBrandModelDTO opEvseBrandModelDTO) {
        log.info(BaseConstant.ADDOPEVSEBRANDMODEL_OPEVSEBRANDMODELDTO, opEvseBrandModelDTO);
        if (ObjectUtils.isEmpty(opEvseBrandModelDTO)) {
            return Result.ofFailed(ResultCodeEnum.BAD_REQUEST.setMessage(BaseConstant.PARAM_IS_NULL));
        }
        return opEvseBrandModelRepository.updateOpEvseBrandModel(opEvseBrandModelDTO);
    }

    /**
     *删除产品型号信息
     */
    @Override
    public Result<Boolean> deleteOpEvseBrandModel(OpEvseBrandModelDTO opEvseBrandModelDTO) {
        if (ObjectUtils.isEmpty(opEvseBrandModelDTO)) {
            return Result.ofFailed(ResultCodeEnum.BAD_REQUEST.setMessage(BaseConstant.PARAM_IS_NULL));
        }
        return Result.ofSucceed(opEvseBrandModelRepository.deleteOpEvseBrandModel(opEvseBrandModelDTO));
    }

    /**
     *批量导入产品型号信息
     */
    @Override
    public Result<Boolean> saveOpEvseBrandModelList(List<OpEvseBrandModelDTO> opEvseBrandModelDTOList) {
        if (CollectionUtils.isEmpty(opEvseBrandModelDTOList)) {
            return Result.ofFailed(ResultCodeEnum.BAD_REQUEST.setMessage(BaseConstant.PARAM_IS_NULL));
        }
        return opEvseBrandModelRepository.saveOpEvseBrandModelList(opEvseBrandModelDTOList);
    }

    @Override
    public Result<Boolean> verifyBrandNameAndProductModel(VerifyBrandNameAndProductModelDTO verifyBrandNameAndProductModelDTO) {
        return opEvseBrandModelRepository.verifyBrandNameAndProductModel(verifyBrandNameAndProductModelDTO);
    }

    @Override
    public Result<Boolean> uploadBrandModelModuleXls(MultipartFile multipartFile) {
        return Result.ofSucceed(opEvseBrandModelRepository.uploadBrandModelModuleXls(multipartFile));
    }

    /**
     * 桩导入模板下载
     *
     * @param request  request
     * @param response response
     * @return Result
     */
    @Override
    public Result<Void> downModuleResourceXls(HttpServletRequest request, HttpServletResponse response) {
        return opEvseBrandModelRepository.downModuleResourceXls(request,response);
    }

    @Override
    public Result<OpEvseBrandModelVO> queryByBrandNameAndProductModel(OpEvseBrandModelDTO opEvseBrandModelDTO) {
        if (ObjectUtils.isEmpty(opEvseBrandModelDTO)) {
            return Result.ofFailed(ResultCodeEnum.BAD_REQUEST.setMessage(BaseConstant.PARAM_IS_NULL));
        }
        return opEvseBrandModelRepository.queryByBrandNameAndProductModel(opEvseBrandModelDTO);
    }
}
