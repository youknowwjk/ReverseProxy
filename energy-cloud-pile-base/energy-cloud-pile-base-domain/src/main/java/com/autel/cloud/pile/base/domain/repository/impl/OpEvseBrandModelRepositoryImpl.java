package com.autel.cloud.pile.base.domain.repository.impl;

import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.EasyExcelFactory;
import com.alibaba.excel.read.listener.PageReadListener;
import com.alibaba.excel.util.FileUtils;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.ResultCodeEnum;
import com.autel.cloud.base.common.util.UserUtil;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.constant.I18nConstant;
import com.autel.cloud.pile.base.domain.repository.OpEvseBrandModelRepository;
import com.autel.cloud.pile.base.dto.BrandModelUploadDTO;
import com.autel.cloud.pile.base.dto.OpEvseBrandModelDTO;
import com.autel.cloud.pile.base.dto.OpLocationConnectorDTO;
import com.autel.cloud.pile.base.dto.VerifyBrandNameAndProductModelDTO;
import com.autel.cloud.pile.base.enums.ConnectorGunTypeEnum;
import com.autel.cloud.pile.base.enums.FileExtensionEnum;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.exception.MessageSourceUtil;
import com.autel.cloud.pile.base.infrastructure.feign.PileUserServiceFeign;
import com.autel.cloud.pile.base.infrastructure.mapper.OpEvseBrandMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpEvseBrandModelMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargeCardExcelEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseBrandEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseBrandModelEntity;
import com.autel.cloud.pile.base.infrastructure.util.StringUtil;
import com.autel.cloud.pile.base.vo.OpEvseBrandModelVO;
import com.autel.cloud.pile.user.api.vo.UserDetailVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellAddress;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.math.BigDecimal;
import java.net.URLEncoder;
import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 桩品牌型号 服务实现类
 * </p>
 *
 * @author A22587
 * @since 2022-09-13
 */

@Service
@Log4j2
public class OpEvseBrandModelRepositoryImpl extends ServiceImpl<OpEvseBrandModelMapper, OpEvseBrandModelEntity> implements OpEvseBrandModelRepository {

    @Autowired
    private OpEvseBrandModelMapper opEvseBrandModelMapper;

    @Autowired
    private OpEvseBrandMapper opEvseBrandMapper;

    @Autowired
    private OpEvseBrandModelRepository opEvseBrandModelRepository;

    @Autowired
    private PileUserServiceFeign pileUserServiceFeign;

    @Autowired
    private MessageSourceUtil messageSourceUtil;

    /**
     * 新增桩产品型号
     *
     * @param opEvseBrandModelDTO
     * @return OpEvseBrandModelVO
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result<OpEvseBrandModelVO> addOpEvseBrandModel(OpEvseBrandModelDTO opEvseBrandModelDTO) {
        Long nowTime = System.currentTimeMillis();
        Long userId = UserUtil.getUserId();
        //校验品牌名是否是道通品牌
        if (BaseConstant.AUTEL.equalsIgnoreCase(opEvseBrandModelDTO.getBrandName())) {
            throw new MessageCodeException(PileBaseEnum.BRAND_NAME_CHECK);
        } else {
            //不是道通的就判断是不是已经存在了
            LambdaQueryWrapper<OpEvseBrandEntity> queryWrapper = new LambdaQueryWrapper<>();
            queryWrapper.eq(OpEvseBrandEntity::getName, opEvseBrandModelDTO.getBrandName())
                    .eq(OpEvseBrandEntity::getDeleted, 0);
            OpEvseBrandEntity opEvseBrandEntity = opEvseBrandMapper.selectOne(queryWrapper);
            if (opEvseBrandEntity != null) {
                //已经存在
                opEvseBrandModelDTO.setBrandName(opEvseBrandEntity.getName());
                opEvseBrandModelDTO.setBrandId(opEvseBrandEntity.getId());
            } else {
                //不存在则添加品牌
                opEvseBrandEntity = new OpEvseBrandEntity();
                opEvseBrandEntity.setName(opEvseBrandModelDTO.getBrandName());
                opEvseBrandEntity.setCreatedAt(nowTime);
                opEvseBrandEntity.setUpdatedAt(nowTime);
                opEvseBrandEntity.setThirdPart(1);
                opEvseBrandEntity.setDeleted(0);
                opEvseBrandMapper.insert(opEvseBrandEntity);
                opEvseBrandModelDTO.setBrandId(opEvseBrandEntity.getId());
            }
        }
        //校验该品牌的品牌型号是否已经存在
        LambdaQueryWrapper<OpEvseBrandModelEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(OpEvseBrandModelEntity::getBrandName, opEvseBrandModelDTO.getBrandName())
                .eq(OpEvseBrandModelEntity::getProductModel, opEvseBrandModelDTO.getProductModel())
                .eq(OpEvseBrandModelEntity::getDeleted, 0);
        Integer integer = this.baseMapper.selectCount(queryWrapper);
        if (integer > 0) {
            throw new MessageCodeException(PileBaseEnum.PRODUCT_MODEL_ALREADY_EXISTS);
        }
        OpEvseBrandModelEntity opEvseBrandModelEntity = new OpEvseBrandModelEntity();
        BeanUtils.copyProperties(opEvseBrandModelDTO, opEvseBrandModelEntity);
        if (!CollectionUtils.isEmpty(opEvseBrandModelDTO.getOpLocationConnectorDTOs())) {
            opEvseBrandModelEntity.setGunTypeList(JSON.toJSONString(opEvseBrandModelDTO.getOpLocationConnectorDTOs()));
        }
        //获取用户名称
        Result<UserDetailVO> userDetailVOResult = pileUserServiceFeign.getUserDetail(userId);
        if (!ObjectUtils.isEmpty(userDetailVOResult.getData())) {
            opEvseBrandModelEntity.setUpdatedByName(userDetailVOResult.getData().getName());
            opEvseBrandModelEntity.setCreatedByName(userDetailVOResult.getData().getName());
        }
        opEvseBrandModelEntity.setCreatTime(nowTime);
        opEvseBrandModelEntity.setUpdateTime(nowTime);
        opEvseBrandModelEntity.setCreatedBy(userId);
        opEvseBrandModelEntity.setUpdatedBy(userId);
        opEvseBrandModelEntity.setDeleted(0);
        this.baseMapper.insert(opEvseBrandModelEntity);
        OpEvseBrandModelVO opEvseBrandModelVO = new OpEvseBrandModelVO();
        BeanUtils.copyProperties(opEvseBrandModelEntity, opEvseBrandModelVO);
        opEvseBrandModelVO.setOpLocationConnectorDTOs(JSON.parseArray(opEvseBrandModelEntity.getGunTypeList(), OpLocationConnectorDTO.class));
        return Result.ofSucceed(opEvseBrandModelVO);
    }

    /**
     * 分页查询产品型号信息
     */
    @Override
    public Result<Page<OpEvseBrandModelVO>> pages(OpEvseBrandModelDTO opEvseBrandModelDTO) {
        Page<OpEvseBrandModelEntity> page = new Page<>(opEvseBrandModelDTO.getPage(), opEvseBrandModelDTO.getPageSize());
        QueryWrapper<OpEvseBrandModelEntity> query = new QueryWrapper<>();
        query.eq("deleted", 0);
        //搜索值(品牌，型号，额定功率，充电类型，添加时间（范围），添加人)
        if (opEvseBrandModelDTO.getStartingTime() != null && opEvseBrandModelDTO.getEndTime() != null) {
            query.ge("create_time", opEvseBrandModelDTO.getStartingTime());
            query.le("create_time", opEvseBrandModelDTO.getEndTime());
        }
        if (StringUtils.isNotBlank(opEvseBrandModelDTO.getSearchValue())) {
            String keyWord = opEvseBrandModelDTO.getSearchValue();
            query.last("and (LOCATE('" + keyWord + "', `brand_name`) > 0 or LOCATE('" + keyWord + "', `product_model`) > 0 or LOCATE('" + keyWord + "', `power_type`) > 0 or LOCATE('" + keyWord + "', `power`) > 0 or LOCATE('" + keyWord + "', `updated_by_name`) > 0)");
        }
        Page<OpEvseBrandModelEntity> opEvseBrandModelEntityPage = this.baseMapper.selectPage(page, query);
        List<OpEvseBrandModelVO> opEvseBrandModelVOs = new ArrayList<>();
        if (opEvseBrandModelEntityPage.getRecords() != null) {
            opEvseBrandModelEntityPage.getRecords().forEach(opEvseBrandModelEntity -> {
                OpEvseBrandModelVO opEvseBrandModelVO = new OpEvseBrandModelVO();
                BeanUtils.copyProperties(opEvseBrandModelEntity, opEvseBrandModelVO);
                if (StringUtils.isNotBlank(opEvseBrandModelEntity.getGunTypeList())) {
                    opEvseBrandModelVO.setOpLocationConnectorDTOs(JSON.parseArray(opEvseBrandModelEntity.getGunTypeList(), OpLocationConnectorDTO.class));
                }
                opEvseBrandModelVOs.add(opEvseBrandModelVO);
            });
        }
        Page<OpEvseBrandModelVO> opEvseBrandModelVOPage = new Page<>(opEvseBrandModelEntityPage.getCurrent(), opEvseBrandModelEntityPage.getSize());
        opEvseBrandModelVOPage.setRecords(opEvseBrandModelVOs);
        opEvseBrandModelVOPage.setTotal(opEvseBrandModelEntityPage.getTotal());
        opEvseBrandModelVOPage.setSize(opEvseBrandModelEntityPage.getSize());
        return Result.ofSucceed(opEvseBrandModelVOPage);
    }

    /**
     * 列表查询（根据品牌条件）
     */
    @Override
    public Result<List<OpEvseBrandModelVO>> queryOpEvseBrandModelByBrand(Long brandId) {
        LambdaQueryWrapper<OpEvseBrandModelEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(OpEvseBrandModelEntity::getBrandId, brandId)
                .eq(OpEvseBrandModelEntity::getDeleted, 0);
        List<OpEvseBrandModelEntity> opEvseBrandModelEntities = this.baseMapper.selectList(queryWrapper);
        List<OpEvseBrandModelVO> opEvseBrandModelVOS = new ArrayList<>();
        if (opEvseBrandModelEntities != null) {
            opEvseBrandModelEntities.forEach(opEvseBrandModelEntity -> {
                OpEvseBrandModelVO opEvseBrandModelVO = new OpEvseBrandModelVO();
                BeanUtils.copyProperties(opEvseBrandModelEntity, opEvseBrandModelVO);
                if (StringUtils.isNotBlank(opEvseBrandModelEntity.getGunTypeList())) {
                    opEvseBrandModelVO.setOpLocationConnectorDTOs(JSON.parseArray(opEvseBrandModelEntity.getGunTypeList(), OpLocationConnectorDTO.class));
                }
                opEvseBrandModelVOS.add(opEvseBrandModelVO);
            });
        }
        return Result.ofSucceed(opEvseBrandModelVOS);
    }

    /**
     * 产品型号详情
     */
    @Override
    public Result<OpEvseBrandModelVO> opEvseBrandModelDetail(Long id) {
        LambdaQueryWrapper<OpEvseBrandModelEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(OpEvseBrandModelEntity::getId, id)
                .eq(OpEvseBrandModelEntity::getDeleted, 0);
        OpEvseBrandModelEntity opEvseBrandModelEntity = this.baseMapper.selectOne(queryWrapper);
        OpEvseBrandModelVO opEvseBrandModelVO = new OpEvseBrandModelVO();
        if (opEvseBrandModelEntity == null) {
            return Result.ofFailed(ResultCodeEnum.CUSTOMER_ERROR.setMessage("This product model does not exist！"));
        }
        BeanUtils.copyProperties(opEvseBrandModelEntity, opEvseBrandModelVO);
        if (StringUtils.isNotBlank(opEvseBrandModelEntity.getGunTypeList())) {
            opEvseBrandModelVO.setOpLocationConnectorDTOs(JSON.parseArray(opEvseBrandModelEntity.getGunTypeList(), OpLocationConnectorDTO.class));
        }
        return Result.ofSucceed(opEvseBrandModelVO);
    }

    /**
     * 编辑产品型号信息
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result<Boolean> updateOpEvseBrandModel(OpEvseBrandModelDTO opEvseBrandModelDTO) {
        Long nowTime = System.currentTimeMillis();
        Long userId = UserUtil.getUserId();
        LambdaQueryWrapper<OpEvseBrandModelEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(OpEvseBrandModelEntity::getId, opEvseBrandModelDTO.getId())
                .eq(OpEvseBrandModelEntity::getDeleted, 0);
        OpEvseBrandModelEntity opEvseBrandModelEntity = this.baseMapper.selectOne(queryWrapper);
        if (opEvseBrandModelEntity != null) {
            //校验该品牌的品牌型号是否已经存在
            LambdaQueryWrapper<OpEvseBrandModelEntity> queryWrapper1 = new LambdaQueryWrapper<>();
            queryWrapper1.eq(OpEvseBrandModelEntity::getBrandName, opEvseBrandModelDTO.getBrandName())
                    .eq(OpEvseBrandModelEntity::getProductModel, opEvseBrandModelDTO.getProductModel())
                    .eq(OpEvseBrandModelEntity::getDeleted, 0);
            OpEvseBrandModelEntity opEvseBrandModelEntity1 = this.baseMapper.selectOne(queryWrapper1);
            if (opEvseBrandModelEntity1 != null && !opEvseBrandModelEntity1.getId().equals(opEvseBrandModelEntity.getId())) {
                throw new MessageCodeException(PileBaseEnum.PRODUCT_MODEL_ALREADY_EXISTS);
            }
            BeanUtils.copyProperties(opEvseBrandModelDTO, opEvseBrandModelEntity);
            if (!CollectionUtils.isEmpty(opEvseBrandModelDTO.getOpLocationConnectorDTOs())) {
                opEvseBrandModelEntity.setGunTypeList(JSON.toJSONString(opEvseBrandModelDTO.getOpLocationConnectorDTOs()));
            }
            //获取用户名称
            Result<UserDetailVO> userDetailVOResult = pileUserServiceFeign.getUserDetail(userId);
            if (!ObjectUtils.isEmpty(userDetailVOResult.getData())) {
                opEvseBrandModelEntity.setUpdatedByName(userDetailVOResult.getData().getName());
            }
            opEvseBrandModelEntity.setUpdateTime(nowTime);
            opEvseBrandModelEntity.setUpdatedBy(userId);
            log.info("opEvseBrandModelEntity: {}", opEvseBrandModelEntity);
            this.baseMapper.updateById(opEvseBrandModelEntity);
        }
        return Result.ofSucceed(Boolean.TRUE);
    }

    /**
     * 删除产品型号信息
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean deleteOpEvseBrandModel(OpEvseBrandModelDTO opEvseBrandModelDTO) {
        //删除前查询该产品型号对应品牌是否只有要删除的这一个
        LambdaQueryWrapper<OpEvseBrandModelEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(OpEvseBrandModelEntity::getBrandId, opEvseBrandModelDTO.getBrandId())
                .eq(OpEvseBrandModelEntity::getDeleted, 0);
        Integer integer = opEvseBrandModelMapper.selectCount(queryWrapper);
        if (integer == 1) {
            //只有这一个，连带着删掉品牌表对应品牌
            opEvseBrandMapper.deleteById(opEvseBrandModelDTO.getBrandId());
        }
        return opEvseBrandModelMapper.deleteById(opEvseBrandModelDTO.getId());
    }

    /**
     * 批量导入产品型号信息
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result<Boolean> saveOpEvseBrandModelList(List<OpEvseBrandModelDTO> opEvseBrandModelDTOList) {
        Long nowTime = System.currentTimeMillis();
        Long userId = UserUtil.getUserId();
        //获取用户名称
        Result<UserDetailVO> userDetailVOResult = pileUserServiceFeign.getUserDetail(userId);
        LambdaQueryWrapper<OpEvseBrandEntity> queryWrapper1 = new LambdaQueryWrapper<>();
        queryWrapper1.eq(OpEvseBrandEntity::getDeleted, 0);
        List<OpEvseBrandEntity> opEvseBrandEntities = opEvseBrandMapper.selectList(queryWrapper1);
        Map<String, Long> opEvseBrandEntityMap = opEvseBrandEntities.stream().collect(Collectors.toMap(OpEvseBrandEntity::getName, OpEvseBrandEntity::getId));
        for (OpEvseBrandModelDTO opEvseBrandModelDTO : opEvseBrandModelDTOList) {
            //校验品牌名是否是道通品牌
            if (BaseConstant.AUTEL.equalsIgnoreCase(opEvseBrandModelDTO.getBrandName())) {
                throw new MessageCodeException(PileBaseEnum.BRAND_NAME_CHECK);
            } else {
                //不是道通的就判断是不是已经存在了
                LambdaQueryWrapper<OpEvseBrandEntity> queryWrapper = new LambdaQueryWrapper<>();
                queryWrapper.eq(OpEvseBrandEntity::getName, opEvseBrandModelDTO.getBrandName())
                        .eq(OpEvseBrandEntity::getDeleted, 0);
                OpEvseBrandEntity opEvseBrandEntity = opEvseBrandMapper.selectOne(queryWrapper);
                if (opEvseBrandEntity != null) {
                    //已经存在
                    opEvseBrandModelDTO.setBrandName(opEvseBrandEntity.getName());
                    opEvseBrandModelDTO.setBrandId(opEvseBrandEntity.getId());
                } else {
                    //不存在则添加品牌
                    opEvseBrandEntity = new OpEvseBrandEntity();
                    opEvseBrandEntity.setName(opEvseBrandModelDTO.getBrandName());
                    opEvseBrandEntity.setCreatedAt(nowTime);
                    opEvseBrandEntity.setUpdatedAt(nowTime);
                    opEvseBrandEntity.setThirdPart(1);
                    opEvseBrandEntity.setDeleted(0);
                    opEvseBrandMapper.insert(opEvseBrandEntity);
                    opEvseBrandModelDTO.setBrandId(opEvseBrandEntity.getId());
                }
            }
            //校验该品牌的品牌型号是否已经存在
            LambdaQueryWrapper<OpEvseBrandModelEntity> queryWrapper = new LambdaQueryWrapper<>();
            queryWrapper.eq(OpEvseBrandModelEntity::getBrandName, opEvseBrandModelDTO.getBrandName())
                    .eq(OpEvseBrandModelEntity::getProductModel, opEvseBrandModelDTO.getProductModel())
                    .eq(OpEvseBrandModelEntity::getDeleted, 0);
            Integer integer = this.baseMapper.selectCount(queryWrapper);
            if (integer > 0) {
                throw new MessageCodeException(PileBaseEnum.PRODUCT_MODEL_ALREADY_EXISTS);
            }
            OpEvseBrandModelEntity opEvseBrandModelEntity = new OpEvseBrandModelEntity();
            BeanUtils.copyProperties(opEvseBrandModelDTO, opEvseBrandModelEntity);
            if (!CollectionUtils.isEmpty(opEvseBrandModelDTO.getOpLocationConnectorDTOs())) {
                opEvseBrandModelEntity.setGunTypeList(JSON.toJSONString(opEvseBrandModelDTO.getOpLocationConnectorDTOs()));
            }
            opEvseBrandModelEntity.setBrandId(opEvseBrandEntityMap.get(opEvseBrandModelDTO.getBrandName()));
            opEvseBrandModelEntity.setCreatTime(nowTime);
            opEvseBrandModelEntity.setUpdateTime(nowTime);
            if (!ObjectUtils.isEmpty(userDetailVOResult.getData())) {
                opEvseBrandModelEntity.setUpdatedByName(userDetailVOResult.getData().getName());
                opEvseBrandModelEntity.setCreatedByName(userDetailVOResult.getData().getName());
            }
            opEvseBrandModelEntity.setCreatedBy(userId);
            opEvseBrandModelEntity.setUpdatedBy(userId);
            opEvseBrandModelEntity.setDeleted(0);
            this.baseMapper.insert(opEvseBrandModelEntity);
        }
        return Result.ofSucceed(Boolean.TRUE);
    }

    @Override
    public Result<Boolean> verifyBrandNameAndProductModel(VerifyBrandNameAndProductModelDTO verifyBrandNameAndProductModelDTO) {
        //校验该品牌的品牌型号是否已经存在
        LambdaQueryWrapper<OpEvseBrandModelEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(OpEvseBrandModelEntity::getBrandName, verifyBrandNameAndProductModelDTO.getBrandName())
                .eq(OpEvseBrandModelEntity::getProductModel, verifyBrandNameAndProductModelDTO.getProductModel())
                .eq(OpEvseBrandModelEntity::getDeleted, 0);
        Integer integer = this.baseMapper.selectCount(queryWrapper);
        if (integer > 0) {
            return Result.ofSucceed(Boolean.TRUE);
        }
        return Result.ofSucceed(Boolean.FALSE);
    }

    @Override
    public Boolean uploadBrandModelModuleXls(MultipartFile multipartFile) {
        //校验文件后缀
        String extension = Objects.requireNonNull(FilenameUtils.getExtension(multipartFile.getOriginalFilename())).toLowerCase();
        if (!Objects.equals(extension, FileExtensionEnum.XLSX.getName()) && !Objects.equals(extension, FileExtensionEnum.XLS.getName()) && !Objects.equals(extension, FileExtensionEnum.CSV.getName())) {
            throw new MessageCodeException(PileBaseEnum.FILE_EXTENSION_WRONG);
        }
        List<BrandModelUploadDTO> brandModelUploadDTOList = new ArrayList<>();
        File file = multipartFile2File(multipartFile);
        EasyExcelFactory.read(file, BrandModelUploadDTO.class, new PageReadListener<BrandModelUploadDTO>(brandModelUploadDTOList::addAll)).sheet().doRead();
        log.info("解析导入产品型号excel数据：{}", JSON.toJSONString(brandModelUploadDTOList));

        //校验数据
        checkData(brandModelUploadDTOList);

        List<OpEvseBrandModelDTO> opEvseBrandModelDTOList = new ArrayList<>();
        for (int i = 0; i <= brandModelUploadDTOList.size() - 1; i++) {
            BrandModelUploadDTO brandModelUploadDTO = brandModelUploadDTOList.get(i);
            OpEvseBrandModelDTO opEvseBrandModelDTO = new OpEvseBrandModelDTO();
            opEvseBrandModelDTO.setBrandName(brandModelUploadDTO.getBrandName());
            opEvseBrandModelDTO.setProductModel(brandModelUploadDTO.getProductModel());
            opEvseBrandModelDTO.setPower(Double.parseDouble(brandModelUploadDTO.getPower()));
            opEvseBrandModelDTO.setPowerType(brandModelUploadDTO.getPowerType());
            opEvseBrandModelDTO.setAmperage(new BigDecimal(brandModelUploadDTO.getAmperage()));
            opEvseBrandModelDTO.setVoltage(new BigDecimal(brandModelUploadDTO.getVoltage()));
            List<OpLocationConnectorDTO> opLocationConnectorDTOs = new ArrayList<>();
            if (!StringUtils.isBlank(brandModelUploadDTO.getConnector1())) {
                OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
                Integer gunType1 = ConnectorGunTypeEnum.getEnumByName(brandModelUploadDTO.getConnector1()).getCode();
                opLocationConnectorDTO.setGunType(gunType1);
                opLocationConnectorDTO.setConnectorId("1");
                opLocationConnectorDTOs.add(opLocationConnectorDTO);
            }
            if (!StringUtils.isBlank(brandModelUploadDTO.getConnector2())) {
                OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
                Integer gunType2 = ConnectorGunTypeEnum.getEnumByName(brandModelUploadDTO.getConnector2()).getCode();
                opLocationConnectorDTO.setGunType(gunType2);
                opLocationConnectorDTO.setConnectorId("2");
                opLocationConnectorDTOs.add(opLocationConnectorDTO);
            }
            if (!StringUtils.isBlank(brandModelUploadDTO.getConnector3())) {
                OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
                Integer gunType3 = ConnectorGunTypeEnum.getEnumByName(brandModelUploadDTO.getConnector3()).getCode();
                opLocationConnectorDTO.setGunType(gunType3);
                opLocationConnectorDTO.setConnectorId("3");
                opLocationConnectorDTOs.add(opLocationConnectorDTO);
            }
            if (!StringUtils.isBlank(brandModelUploadDTO.getConnector4())) {
                OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
                Integer gunType4 = ConnectorGunTypeEnum.getEnumByName(brandModelUploadDTO.getConnector3()).getCode();
                opLocationConnectorDTO.setGunType(gunType4);
                opLocationConnectorDTO.setConnectorId("4");
                opLocationConnectorDTOs.add(opLocationConnectorDTO);
            }
            opEvseBrandModelDTO.setOpLocationConnectorDTOs(opLocationConnectorDTOs);
            opEvseBrandModelDTOList.add(opEvseBrandModelDTO);
        }
        //批量导入产品型号表
        log.info("保存:{}",JSON.toJSONString(opEvseBrandModelDTOList));
        return opEvseBrandModelRepository.saveOpEvseBrandModelList(opEvseBrandModelDTOList).getData();
    }

    @Override
    public Result<OpEvseBrandModelVO> queryByBrandNameAndProductModel(OpEvseBrandModelDTO opEvseBrandModelDTO) {
        OpEvseBrandModelVO opEvseBrandModelVO = new OpEvseBrandModelVO();
        LambdaQueryWrapper<OpEvseBrandModelEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(OpEvseBrandModelEntity::getBrandName, opEvseBrandModelDTO.getBrandName())
                .eq(OpEvseBrandModelEntity::getProductModel, opEvseBrandModelDTO.getProductModel())
                .eq(OpEvseBrandModelEntity::getDeleted, 0);
        OpEvseBrandModelEntity opEvseBrandModelEntity = this.baseMapper.selectOne(queryWrapper);
        if (!ObjectUtils.isEmpty(opEvseBrandModelEntity)) {
            opEvseBrandModelVO.setAmperage(opEvseBrandModelEntity.getAmperage());
            opEvseBrandModelVO.setVoltage(opEvseBrandModelEntity.getVoltage());
        }
        return Result.ofSucceed(opEvseBrandModelVO);
    }

    private File multipartFile2File(MultipartFile multipartFile) {
        File file = null;
        try {
            String fileName = multipartFile.getOriginalFilename();
            assert fileName != null;
            file = File.createTempFile(fileName.substring(0, fileName.lastIndexOf(".")), fileName.substring(fileName.lastIndexOf(".")));
            multipartFile.transferTo(file);
        } catch (IOException e) {
            log.error("multipartFile2File:" + e);
        }
        return file;
    }

    private void checkData(List<BrandModelUploadDTO> brandModelUploadDTOList) {

        //校验是否上传空数据
        if (org.apache.commons.collections4.CollectionUtils.isEmpty(brandModelUploadDTOList)) {
            throw new MessageCodeException(PileBaseEnum.EMPTY_FILE);
        }
        //删除全部空白数据
        for (int i = brandModelUploadDTOList.size() - 1; i >= 0; i--) {
            BrandModelUploadDTO brandModelUploadDTO = brandModelUploadDTOList.get(i);
            if (StringUtils.isBlank(brandModelUploadDTO.getBrandName()) && StringUtils.isBlank(brandModelUploadDTO.getProductModel()) && StringUtils.isBlank(brandModelUploadDTO.getPower()) && StringUtils.isBlank(brandModelUploadDTO.getPowerType()) && StringUtils.isBlank(brandModelUploadDTO.getAmperage()) && StringUtils.isBlank(brandModelUploadDTO.getVoltage())) {
                brandModelUploadDTOList.remove(i);
            }
        }
        //校验是否上传空数据
        if (org.apache.commons.collections4.CollectionUtils.isEmpty(brandModelUploadDTOList)) {
            throw new MessageCodeException(PileBaseEnum.PRODUCT_MODULE_COMPLETE_DATA);
        }

        Map<String, Integer> brandNameAndProductModelCountMap = buildSNToSNCountMap(brandModelUploadDTOList);

        for (int i = 0; i <= brandModelUploadDTOList.size() - 1; i++) {
            BrandModelUploadDTO brandModelUploadDTO = brandModelUploadDTOList.get(i);
            //1、必填字段校验 品牌/产品型号/充电类型/额定功率/电流/电压/枪类型必填
            if (StringUtils.isBlank(brandModelUploadDTO.getBrandName()) ||
                    StringUtils.isBlank(brandModelUploadDTO.getProductModel()) ||
                    StringUtils.isBlank(brandModelUploadDTO.getPowerType()) ||
                    StringUtils.isBlank(brandModelUploadDTO.getPower()) ||
                    StringUtils.isBlank(brandModelUploadDTO.getAmperage()) ||
                    StringUtils.isBlank(brandModelUploadDTO.getVoltage()) ||
                    (StringUtils.isBlank(brandModelUploadDTO.getConnector1()) &&
                            StringUtils.isBlank(brandModelUploadDTO.getConnector2()) &&
                            StringUtils.isBlank(brandModelUploadDTO.getConnector3())) &&
                            StringUtils.isBlank(brandModelUploadDTO.getConnector4())
            ) {
                throw new MessageCodeException(PileBaseEnum.PRODUCT_MODULE_COMPLETE_DATA);
            }

            //2、校验文件中品牌名对应的产品型号是否唯一
            String brandNameAndProductModel = brandModelUploadDTO.getBrandName() + brandModelUploadDTO.getProductModel();
            Integer integer = brandNameAndProductModelCountMap.get(brandNameAndProductModel);
            if (integer > 1) {
                throw new MessageCodeException(PileBaseEnum.PRODUCT_MODEL_ALREADY_EXISTS);
            }
            //3、校验品牌名是否是道通品牌
            if (BaseConstant.AUTEL.equalsIgnoreCase(brandModelUploadDTO.getBrandName())) {
                throw new MessageCodeException(PileBaseEnum.BRAND_NAME_CHECK);
            }
            //4、不是道通的就判断是不是已经存在了
            LambdaQueryWrapper<OpEvseBrandEntity> queryWrapper = new LambdaQueryWrapper<>();
            queryWrapper.eq(OpEvseBrandEntity::getName, brandModelUploadDTO.getBrandName())
                    .eq(OpEvseBrandEntity::getDeleted, 0);
            OpEvseBrandEntity opEvseBrandEntity = opEvseBrandMapper.selectOne(queryWrapper);
            if (opEvseBrandEntity != null) {
                //已经存在
                brandModelUploadDTO.setBrandName(opEvseBrandEntity.getName());
            } else {
                Long nowTime = System.currentTimeMillis();
                //不存在则添加品牌
                opEvseBrandEntity = new OpEvseBrandEntity();
                opEvseBrandEntity.setName(brandModelUploadDTO.getBrandName());
                opEvseBrandEntity.setCreatedAt(nowTime);
                opEvseBrandEntity.setUpdatedAt(nowTime);
                opEvseBrandEntity.setThirdPart(1);
                opEvseBrandEntity.setDeleted(0);
                opEvseBrandMapper.insert(opEvseBrandEntity);
            }
            //5、校验该品牌的品牌型号在数据库中是否已经存在
            LambdaQueryWrapper<OpEvseBrandModelEntity> queryWrapper1 = new LambdaQueryWrapper<>();
            queryWrapper1.eq(OpEvseBrandModelEntity::getBrandName, brandModelUploadDTO.getBrandName())
                    .eq(OpEvseBrandModelEntity::getProductModel, brandModelUploadDTO.getProductModel())
                    .eq(OpEvseBrandModelEntity::getDeleted, 0);
            Integer integer5 = this.baseMapper.selectCount(queryWrapper1);
            if (integer5 > 0) {
                throw new MessageCodeException(PileBaseEnum.PRODUCT_MODEL_ALREADY_EXISTS);
            }
        }
    }

    //校验文件中品牌名对应的产品型号是否唯一
    private Map<String, Integer> buildSNToSNCountMap(List<BrandModelUploadDTO> brandModelUploadDTOList) {
        Map<String, Integer> brandNameAndProductModelCountMap = new HashMap<>();
        brandModelUploadDTOList.forEach(brandModelUploadDTO -> {
            if (StringUtils.isNotBlank(brandModelUploadDTO.getBrandName()) && StringUtils.isNotBlank(brandModelUploadDTO.getProductModel())) {
                String brandNameAndProductModel = brandModelUploadDTO.getBrandName() + brandModelUploadDTO.getProductModel();
                Integer productModelCount = brandNameAndProductModelCountMap.get(brandNameAndProductModel);
                if (productModelCount == null) {
                    productModelCount = 0;
                }
                productModelCount++;
                brandNameAndProductModelCountMap.put(brandNameAndProductModel, productModelCount);
            }
        });
        return brandNameAndProductModelCountMap;
    }

    /**
     * 三方桩导入模板下载
     *
     * @param request  request
     * @param response response
     * @return Result
     */
    @Override
    public Result<Void> downModuleResourceXls(HttpServletRequest request, HttpServletResponse response) {
        String downLoadFileName = messageSourceUtil.getMessage(I18nConstant.ThirdPartModule.FILE_NAME);

        String brandName = messageSourceUtil.getMessage(I18nConstant.ThirdPartModule.title.BRAND_NAME);
        String productModel = messageSourceUtil.getMessage(I18nConstant.ThirdPartModule.title.PRODUCT_MODEL);
        String chargePower = messageSourceUtil.getMessage(I18nConstant.ThirdPartModule.title.POWER);
        String chargeType = messageSourceUtil.getMessage(I18nConstant.ThirdPartModule.title.POWER_TYPE);
        String amperage = messageSourceUtil.getMessage(I18nConstant.ThirdPartModule.title.AMPERAGE);
        String voltage = messageSourceUtil.getMessage(I18nConstant.ThirdPartModule.title.VOLTAGE);
        String connector1 = messageSourceUtil.getMessage(I18nConstant.ThirdPartModule.title.CONNECTOR1);
        String connector2 = messageSourceUtil.getMessage(I18nConstant.ThirdPartModule.title.CONNECTOR2);
        String connector3 = messageSourceUtil.getMessage(I18nConstant.ThirdPartModule.title.CONNECTOR3);
        String connector4 = messageSourceUtil.getMessage(I18nConstant.ThirdPartModule.title.CONNECTOR4);
        String[] titleArray = {brandName, productModel, chargePower, chargeType, amperage, voltage, connector1, connector2, connector3, connector4};
        log.info("下载文件名称：{}  表头数组：{}", downLoadFileName, JSON.toJSONString(titleArray));
        Workbook workbook = null;
        FileInputStream fileInputStream = null;
        OutputStream outputStream = null;
        try {
            String resourceFileName = "三方桩导入模板.xlsx";
            String resourceFilePath = "xls/" + resourceFileName;
            InputStream inputStream = ChargeCardExcelEntity.class.getClassLoader().getResourceAsStream(resourceFilePath);
            File tempFile = File.createTempFile("test", ".xlsx");
            FileUtils.writeToFile(tempFile, inputStream);
            //sheet名称、标题国际化
            fileInputStream = new FileInputStream(tempFile.getPath());
            workbook = new XSSFWorkbook(fileInputStream);
            outputStream = new FileOutputStream(tempFile.getPath());
            Sheet sheet = workbook.getSheetAt(0);
            workbook.setSheetName(0,"Sheet1");
            sheet.setActiveCell(new CellAddress(0, 0));
            Row row = sheet.getRow(0);
            for (int i = 0; i < titleArray.length; i++) {
                Cell cell = row.getCell(i);
                cell.setCellValue(titleArray[i]);
            }
            workbook.write(outputStream);
            //下载
            response.setContentType("application/vnd.ms-excel");
            response.setCharacterEncoding("utf-8");
            downLoadFileName = URLEncoder.encode(downLoadFileName, "UTF-8").replaceAll("\\+", "%20");
            response.setHeader("Content-disposition", "attachment;filename*=utf-8''" + downLoadFileName + ".xlsx");
            byte[] fileToByteArray = FileUtils.readFileToByteArray(tempFile);
            response.getOutputStream().write(fileToByteArray);
        } catch (IOException e) {
            log.error("downModuleResourceXls:" + e);
        } finally {
           if(workbook != null) {
               try {
                   workbook.close();
               } catch (IOException e) {
                   log.error("OpEvseBrandModelRepositoryImpl.downModuleResourceXls close workbook exception = ", e);
               }
           }
           if(fileInputStream != null) {
               try {
                   fileInputStream.close();
               } catch (IOException e) {
                   log.error("OpEvseBrandModelRepositoryImpl.downModuleResourceXls close fileInputStream exception = ", e);
               }
           }
           if(outputStream != null) {
               try {
                   outputStream.close();
               } catch (IOException e) {
                   log.error("OpEvseBrandModelRepositoryImpl.downModuleResourceXls close outputStream exception = ", e);
               }
           }
        }

        return null;
    }

    @Override
    public List<String> getModelListByBrandName(String brandName) {

        if (StringUtils.isBlank(brandName)) {
            return null;
        }
        return opEvseBrandMapper.getModelListByBrandName(brandName);
    }
}
