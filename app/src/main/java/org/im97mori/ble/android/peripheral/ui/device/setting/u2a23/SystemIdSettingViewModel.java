package org.im97mori.ble.android.peripheral.ui.device.setting.u2a23;

import static org.im97mori.ble.android.peripheral.utils.Utils.stackLog;
import static org.im97mori.ble.constants.CharacteristicUUID.SYSTEM_ID_CHARACTERISTIC;

import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCharacteristic;
import android.content.Intent;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.SavedStateHandle;
import androidx.lifecycle.Transformations;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseCharacteristicViewModel;
import org.im97mori.ble.characteristic.u2a23.SystemId;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class SystemIdSettingViewModel extends BaseCharacteristicViewModel {

    private static final String KEY_IS_ERROR_RESPONSE = "KEY_IS_ERROR_RESPONSE";

    private static final String KEY_MANUFACTURER_IDENTIFIER = "KEY_MANUFACTURER_IDENTIFIER";
    private static final String KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER = "KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER";
    private static final String KEY_RESPONSE_CODE = "KEY_RESPONSE_CODE";
    private static final String KEY_RESPONSE_DELAY = "KEY_RESPONSE_DELAY";

    private final MutableLiveData<Boolean> mIsErrorResponse;

    private final MutableLiveData<String> mManufacturerIdentifier;
    private final MutableLiveData<String> mOrganizationallyUniqueIdentifier;
    private final MutableLiveData<String> mResponseCode;
    private final MutableLiveData<String> mResponseDelay;

    private final MutableLiveData<Intent> mSavedData;

    @Inject
    public SystemIdSettingViewModel(@NonNull SavedStateHandle savedStateHandle, @NonNull DeviceSettingRepository deviceSettingRepository, @NonNull Gson gson) {
        super(deviceSettingRepository, gson);

        mIsErrorResponse = savedStateHandle.getLiveData(KEY_IS_ERROR_RESPONSE);
        mManufacturerIdentifier = savedStateHandle.getLiveData(KEY_MANUFACTURER_IDENTIFIER);
        mOrganizationallyUniqueIdentifier = savedStateHandle.getLiveData(KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER);
        mResponseCode = savedStateHandle.getLiveData(KEY_RESPONSE_CODE);
        mResponseDelay = savedStateHandle.getLiveData(KEY_RESPONSE_DELAY);

        mSavedData = savedStateHandle.getLiveData(KEY_SAVED_DATA);
    }

    @Override
    public void observeSetup(@NonNull Intent intent
            , @NonNull Action onComplete
            , @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Completable.create(emitter -> {
            if (mCharacteristicData == null) {
                try {
                    mCharacteristicData = mGson.fromJson(intent.getStringExtra(SYSTEM_ID_CHARACTERISTIC.toString())
                            , CharacteristicData.class);
                } catch (JsonSyntaxException e) {
                    stackLog(e);
                }

                if (mCharacteristicData == null) {
                    mCharacteristicData = new CharacteristicData();
                    mCharacteristicData.uuid = SYSTEM_ID_CHARACTERISTIC;
                    mCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
                    mCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
                }
            }

            if (mIsErrorResponse.getValue() == null) {
                mIsErrorResponse.postValue(mCharacteristicData.responseCode != BluetoothGatt.GATT_SUCCESS);
            }

            SystemId systemId;
            if (mCharacteristicData.data == null) {
                systemId = null;
            } else {
                systemId = new SystemId(mCharacteristicData.data);
            }

            if (mManufacturerIdentifier.getValue() == null) {
                if (systemId == null) {
                    mManufacturerIdentifier.postValue(null);
                } else {
                    mManufacturerIdentifier.postValue(String.valueOf(systemId.getManufacturerIdentifier()));
                }
            }

            if (mOrganizationallyUniqueIdentifier.getValue() == null) {
                if (systemId == null) {
                    mOrganizationallyUniqueIdentifier.postValue(null);
                } else {
                    mOrganizationallyUniqueIdentifier.postValue(String.valueOf(systemId.getOrganizationallyUniqueIdentifier()));
                }
            }

            if (mResponseCode.getValue() == null) {
                mResponseCode.postValue(String.valueOf(mCharacteristicData.responseCode));
            }

            if (mResponseDelay.getValue() == null) {
                mResponseDelay.postValue(String.valueOf(mCharacteristicData.delay));
            }

            emitter.onComplete();
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onComplete, onError));
    }

    @MainThread
    public void observeIsErrorResponse(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIsErrorResponse).observe(owner, observer);
    }

    @MainThread
    public void observeManufacturerIdentifier(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mManufacturerIdentifier).observe(owner, observer);
    }

    @MainThread
    public void observeManufacturerIdentifierErrorString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mManufacturerIdentifier).observe(owner
                , s -> observer.onChanged(mDeviceSettingRepository.getManufacturerIdentifierErrorString(s)));
    }

    @MainThread
    public void observeOrganizationallyUniqueIdentifier(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mOrganizationallyUniqueIdentifier).observe(owner, observer);
    }

    @MainThread
    public void observeOrganizationallyUniqueIdentifierErrorString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mOrganizationallyUniqueIdentifier).observe(owner
                , s -> observer.onChanged(mDeviceSettingRepository.getOrganizationallyUniqueIdentifierErrorString(s)));
    }

    @MainThread
    public void observeResponseCode(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseCode).observe(owner, observer);
    }

    @MainThread
    public void observeResponseCodeErrorString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseCode).observe(owner
                , s -> observer.onChanged(mDeviceSettingRepository.getResponseCodeErrorString(s)));
    }

    @MainThread
    public void observeResponseDelay(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseDelay).observe(owner, observer);
    }

    @MainThread
    public void observeResponseDelayErrorString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseDelay).observe(owner
                , s -> observer.onChanged(mDeviceSettingRepository.getResponseDelayErrorString(s)));
    }

    @Override
    public void observeSavedData(@NonNull LifecycleOwner owner, @NonNull Observer<Intent> observer) {
        mSavedData.observe(owner, observer);
    }

    @MainThread
    public void updateIsErrorResponse(boolean checked) {
        mIsErrorResponse.setValue(checked);
    }

    @MainThread
    public void updateManufacturerIdentifier(@NonNull String text) {
        mManufacturerIdentifier.setValue(text);
    }

    @MainThread
    public void updateOrganizationallyUniqueIdentifier(@NonNull String text) {
        mOrganizationallyUniqueIdentifier.setValue(text);
    }

    @MainThread
    public void updateResponseCode(@NonNull String text) {
        mResponseCode.setValue(text);
    }

    @MainThread
    public void updateResponseDelay(@NonNull String text) {
        mResponseDelay.setValue(text);
    }

    @Override
    public void save(@NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Completable.create(emitter -> {
            if (mCharacteristicData == null) {
                emitter.onError(new RuntimeException("Already saved"));
            } else {
                boolean isErrorResponse = Boolean.TRUE.equals(mIsErrorResponse.getValue());
                String responseCode = mResponseCode.getValue();
                String responseDelay = mResponseDelay.getValue();
                String manufacturerIdentifier = mManufacturerIdentifier.getValue();
                String organizationallyUniqueIdentifier = mOrganizationallyUniqueIdentifier.getValue();

                if (responseDelay != null && mDeviceSettingRepository.getResponseDelayErrorString(responseDelay) == null) {
                    mCharacteristicData.delay = Long.parseLong(responseDelay);
                    if (isErrorResponse) {
                        if (responseCode != null && mDeviceSettingRepository.getResponseCodeErrorString(responseCode) == null) {
                            mCharacteristicData.data = null;
                            mCharacteristicData.responseCode = Integer.parseInt(responseCode);

                            Intent intent = new Intent();
                            intent.putExtra(SYSTEM_ID_CHARACTERISTIC.toString(), mGson.toJson(mCharacteristicData));

                            mSavedData.postValue(intent);
                            mCharacteristicData = null;
                            emitter.onComplete();
                        } else {
                            emitter.onError(new RuntimeException("Validation failed"));
                        }
                    } else {
                        if (manufacturerIdentifier != null && mDeviceSettingRepository.getManufacturerNameStringErrorString(manufacturerIdentifier) == null
                                && organizationallyUniqueIdentifier != null && mDeviceSettingRepository.getOrganizationallyUniqueIdentifierErrorString(organizationallyUniqueIdentifier) == null) {
                            mCharacteristicData.data = new SystemId(Long.parseLong(manufacturerIdentifier), Integer.parseInt(organizationallyUniqueIdentifier)).getBytes();
                            mCharacteristicData.responseCode = BluetoothGatt.GATT_SUCCESS;

                            Intent intent = new Intent();
                            intent.putExtra(SYSTEM_ID_CHARACTERISTIC.toString(), mGson.toJson(mCharacteristicData));

                            mSavedData.postValue(intent);
                            mCharacteristicData = null;
                            emitter.onComplete();
                        } else {
                            emitter.onError(new RuntimeException("Validation failed"));
                        }
                    }
                } else {
                    emitter.onError(new RuntimeException("Validation failed"));
                }
            }
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(() -> {
                }, onError));
    }

}