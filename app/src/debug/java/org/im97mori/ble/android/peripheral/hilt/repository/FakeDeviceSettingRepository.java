package org.im97mori.ble.android.peripheral.hilt.repository;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;

import java.util.List;
import java.util.function.Consumer;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.CompletableOnSubscribe;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.processors.PublishProcessor;

public class FakeDeviceSettingRepository extends DeviceSettingRepository {

    public PublishProcessor<List<DeviceSetting>> mLoadAllDeviceSettingProcessor;

    public PublishProcessor<DeviceSetting> mLoadDeviceSettingByIdProcessor;

    public CompletableOnSubscribe mDeleteAllDeviceSettingSubscribe;

    public Consumer<DeviceSetting> mDeleteDeviceSettingConsumer;

    public String mGetDeviceSettingNameErrorString;
    public String mGetModelNumberStringErrorString;
    public String mGetManufacturerNameStringErrorString;
    public String mGetResponseCodeErrorString;
    public String mGetResponseDelayErrorString;
    public String mGetManufacturerIdentifierErrorString;
    public String mGetOrganizationallyUniqueIdentifierErrorString;

    public Consumer<DeviceSetting> mInsertDeviceSettingConsumer;

    @Inject
    public FakeDeviceSettingRepository(@NonNull DeviceSettingDataSource deviceSettingDataSource
            , @NonNull @ApplicationContext Context context) {
        super(deviceSettingDataSource, context);
    }

    @NonNull
    @Override
    public Flowable<List<DeviceSetting>> loadAllDeviceSetting() {
        return Flowable.fromPublisher(mLoadAllDeviceSettingProcessor);
    }

    @Override
    public Single<DeviceSetting> loadDeviceSettingById(long id) {
        return Single.fromPublisher(mLoadDeviceSettingByIdProcessor);
    }

    @NonNull
    @Override
    public Completable insertDeviceSetting(@NonNull DeviceSetting deviceSetting) {
        return Completable.create(emitter -> {
            mInsertDeviceSettingConsumer.accept(deviceSetting);
            emitter.onComplete();
        });
    }

    @NonNull
    @Override
    public Completable deleteDeviceSetting(@NonNull DeviceSetting deviceSetting) {
        return Completable.create(emitter -> {
            mDeleteDeviceSettingConsumer.accept(deviceSetting);
            emitter.onComplete();
        });
    }

    @NonNull
    @Override
    public Completable deleteAllDeviceSetting() {
        return Completable.create(mDeleteAllDeviceSettingSubscribe);
    }

    @Nullable
    @Override
    public String getDeviceSettingNameErrorString(@Nullable CharSequence charSequence) {
        if (mGetDeviceSettingNameErrorString == null) {
            return super.getDeviceSettingNameErrorString(charSequence);
        } else {
            return mGetDeviceSettingNameErrorString;
        }
    }

    @Nullable
    @Override
    public String getResponseDelayErrorString(@Nullable CharSequence charSequence) {
        if (mGetResponseDelayErrorString == null) {
            return super.getResponseDelayErrorString(charSequence);
        } else {
            return mGetResponseDelayErrorString;
        }
    }

    @Nullable
    @Override
    public String getResponseCodeErrorString(@Nullable CharSequence charSequence) {
        if (mGetResponseCodeErrorString == null) {
            return super.getResponseCodeErrorString(charSequence);
        } else {
            return mGetResponseCodeErrorString;
        }
    }

    @Nullable
    @Override
    public String getManufacturerNameStringErrorString(@Nullable CharSequence charSequence) {
        if (mGetManufacturerNameStringErrorString == null) {
            return super.getManufacturerNameStringErrorString(charSequence);
        } else {
            return mGetManufacturerNameStringErrorString;
        }
    }

    @Nullable
    @Override
    public String getModelNumberStringErrorString(@Nullable CharSequence charSequence) {
        if (mGetModelNumberStringErrorString == null) {
            return super.getModelNumberStringErrorString(charSequence);
        } else {
            return mGetModelNumberStringErrorString;
        }
    }

    @Nullable
    @Override
    public String getManufacturerIdentifierErrorString(@Nullable CharSequence charSequence) {
        if (mGetManufacturerIdentifierErrorString == null) {
            return super.getManufacturerIdentifierErrorString(charSequence);
        } else {
            return mGetManufacturerIdentifierErrorString;
        }
    }

    @Nullable
    @Override
    public String getOrganizationallyUniqueIdentifierErrorString(@Nullable CharSequence charSequence) {
        if (mGetOrganizationallyUniqueIdentifierErrorString == null) {
            return super.getOrganizationallyUniqueIdentifierErrorString(charSequence);
        } else {
            return mGetOrganizationallyUniqueIdentifierErrorString;
        }
    }

}
