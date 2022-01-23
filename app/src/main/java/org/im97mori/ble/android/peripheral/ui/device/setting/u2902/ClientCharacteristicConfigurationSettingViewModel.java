package org.im97mori.ble.android.peripheral.ui.device.setting.u2902;

import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_PROPERTIES_TYPE;
import static org.im97mori.ble.android.peripheral.utils.Utils.stackLog;
import static org.im97mori.ble.constants.DescriptorUUID.CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;

import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCharacteristic;
import android.bluetooth.BluetoothGattDescriptor;
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

import org.im97mori.ble.DescriptorData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseDescriptorSettingViewModel;
import org.im97mori.ble.descriptor.u2902.ClientCharacteristicConfiguration;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class ClientCharacteristicConfigurationSettingViewModel extends BaseDescriptorSettingViewModel {

    private static final String KEY_IS_ERROR_RESPONSE = "KEY_IS_ERROR_RESPONSE";

    private static final String KEY_PROPERTIES = "KEY_PROPERTIES";
    private static final String KEY_PROPERTIES_DISABLED = "KEY_PROPERTIES_DISABLED";
    private static final String KEY_PROPERTIES_ENABLED = "KEY_PROPERTIES_ENABLED";
    private static final String KEY_RESPONSE_CODE = "KEY_RESPONSE_CODE";
    private static final String KEY_RESPONSE_DELAY = "KEY_RESPONSE_DELAY";

    private final SavedStateHandle mSavedStateHandle;

    private final MutableLiveData<Boolean> mIsErrorResponse;

    private final MutableLiveData<Boolean> mProperties;

    private final MutableLiveData<String> mResponseCode;
    private final MutableLiveData<String> mResponseDelay;

    private final MutableLiveData<Intent> mSavedData;

    private int mPropertyType;

    @Inject
    public ClientCharacteristicConfigurationSettingViewModel(@NonNull SavedStateHandle savedStateHandle
            , @NonNull DeviceSettingRepository deviceSettingRepository
            , @NonNull Gson gson) {
        super(deviceSettingRepository, gson);

        mSavedStateHandle = savedStateHandle;

        mIsErrorResponse = savedStateHandle.getLiveData(KEY_IS_ERROR_RESPONSE);

        mProperties = savedStateHandle.getLiveData(KEY_PROPERTIES);

        mResponseCode = savedStateHandle.getLiveData(KEY_RESPONSE_CODE);
        mResponseDelay = savedStateHandle.getLiveData(KEY_RESPONSE_DELAY);

        mSavedData = savedStateHandle.getLiveData(KEY_SAVED_DATA);
    }

    @Override
    public void observeSetup(@NonNull Intent intent
            , @NonNull Action onComplete
            , @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Completable.create(emitter -> {
            if (mDescriptorData == null) {
                try {
                    mDescriptorData = mGson.fromJson(intent.getStringExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString())
                            , DescriptorData.class);
                } catch (JsonSyntaxException e) {
                    stackLog(e);
                }

                if (mDescriptorData == null) {
                    mDescriptorData = new DescriptorData();
                    mDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
                    mDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
                }
            }

            if (mIsErrorResponse.getValue() == null) {
                mIsErrorResponse.postValue(mDescriptorData.responseCode != BluetoothGatt.GATT_SUCCESS);
            }

            ClientCharacteristicConfiguration clientCharacteristicConfiguration;
            if (mDescriptorData.data != null) {
                clientCharacteristicConfiguration = new ClientCharacteristicConfiguration(mDescriptorData.data);
            } else {
                clientCharacteristicConfiguration = null;
            }

            mPropertyType = intent.getIntExtra(KEY_PROPERTIES_TYPE, BluetoothGattCharacteristic.PROPERTY_NOTIFY);
            if (clientCharacteristicConfiguration == null) {
                if (mProperties.getValue() == null) {
                    mProperties.postValue(false);
                    if (BluetoothGattCharacteristic.PROPERTY_NOTIFY == mPropertyType) {
                        mSavedStateHandle.<String>getLiveData(KEY_PROPERTIES_DISABLED)
                                .postValue(mDeviceSettingRepository.getNotificationsDisabledString());
                        mSavedStateHandle.<String>getLiveData(KEY_PROPERTIES_ENABLED)
                                .postValue(mDeviceSettingRepository.getNotificationsEnabledString());
                    } else {
                        mSavedStateHandle.<String>getLiveData(KEY_PROPERTIES_DISABLED)
                                .postValue(mDeviceSettingRepository.getIndicationsDisabledString());
                        mSavedStateHandle.<String>getLiveData(KEY_PROPERTIES_ENABLED)
                                .postValue(mDeviceSettingRepository.getIndicationsEnabledString());
                    }
                }
            } else {
                if (mProperties.getValue() == null) {
                    if (BluetoothGattCharacteristic.PROPERTY_NOTIFY == mPropertyType) {
                        mProperties.postValue(clientCharacteristicConfiguration.isPropertiesNotificationsEnabled());
                        mSavedStateHandle.<String>getLiveData(KEY_PROPERTIES_DISABLED)
                                .postValue(mDeviceSettingRepository.getNotificationsDisabledString());
                        mSavedStateHandle.<String>getLiveData(KEY_PROPERTIES_ENABLED)
                                .postValue(mDeviceSettingRepository.getNotificationsEnabledString());
                    } else {
                        mProperties.postValue(clientCharacteristicConfiguration.isPropertiesIndicationsEnabled());
                        mSavedStateHandle.<String>getLiveData(KEY_PROPERTIES_DISABLED)
                                .postValue(mDeviceSettingRepository.getIndicationsDisabledString());
                        mSavedStateHandle.<String>getLiveData(KEY_PROPERTIES_ENABLED)
                                .postValue(mDeviceSettingRepository.getIndicationsEnabledString());
                    }
                }
            }

            if (mResponseCode.getValue() == null) {
                mResponseCode.postValue(String.valueOf(mDescriptorData.responseCode));
            }

            if (mResponseDelay.getValue() == null) {
                mResponseDelay.postValue(String.valueOf(mDescriptorData.delay));
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
    public void observeProperties(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mProperties).observe(owner, observer);
    }

    @MainThread
    public void observePropertiesDisabled(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_PROPERTIES_DISABLED)).observe(owner, observer);
    }

    @MainThread
    public void observePropertiesEnabled(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_PROPERTIES_ENABLED)).observe(owner, observer);
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

    @MainThread
    public void observeSavedData(@NonNull LifecycleOwner owner, @NonNull Observer<Intent> observer) {
        mSavedData.observe(owner, observer);
    }

    @MainThread
    public void updateIsErrorResponse(boolean checked) {
        mIsErrorResponse.setValue(checked);
    }

    @MainThread
    public void updateProperties(boolean checked) {
        mProperties.setValue(checked);
    }

    @MainThread
    public void updateResponseDelay(@NonNull String text) {
        mResponseDelay.setValue(text);
    }

    @MainThread
    public void updateResponseCode(@NonNull String text) {
        mResponseCode.setValue(text);
    }

    @Override
    public void save(@NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Completable.create(emitter -> {
            if (mDescriptorData == null) {
                emitter.onError(new RuntimeException("Already saved"));
            } else {
                boolean isErrorResponse = Boolean.TRUE.equals(mIsErrorResponse.getValue());
                String responseCode = mResponseCode.getValue();
                String responseDelay = mResponseDelay.getValue();
                boolean properties = Boolean.TRUE.equals(mProperties.getValue());

                if (responseDelay != null && mDeviceSettingRepository.getResponseDelayErrorString(responseDelay) == null) {
                    mDescriptorData.delay = Long.parseLong(responseDelay);
                    if (isErrorResponse) {
                        if (responseCode != null && mDeviceSettingRepository.getResponseCodeErrorString(responseCode) == null) {
                            mDescriptorData.data = null;
                            mDescriptorData.responseCode = Integer.parseInt(responseCode);

                            Intent intent = new Intent();
                            intent.putExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), mGson.toJson(mDescriptorData));

                            mSavedData.postValue(intent);
                            mDescriptorData = null;
                            emitter.onComplete();
                        } else {
                            emitter.onError(new RuntimeException("Validation failed"));
                        }
                    } else {
                        byte[] data;
                        if (properties) {
                            if (BluetoothGattCharacteristic.PROPERTY_NOTIFY == mPropertyType) {
                                data = BluetoothGattDescriptor.ENABLE_NOTIFICATION_VALUE;
                            } else {
                                data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
                            }
                        } else {
                            data = BluetoothGattDescriptor.DISABLE_NOTIFICATION_VALUE;
                        }
                        mDescriptorData.data = new ClientCharacteristicConfiguration(data).getBytes();
                        mDescriptorData.responseCode = BluetoothGatt.GATT_SUCCESS;

                        Intent intent = new Intent();
                        intent.putExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), mGson.toJson(mDescriptorData));

                        mSavedData.postValue(intent);
                        mDescriptorData = null;
                        emitter.onComplete();
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